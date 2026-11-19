# ADR 2026-11-19 : erreur standard des effets aléatoires de `lmer` et `glmer` par la hessienne de la déviance

Statut : approuvé par Amal le 2026-11-19. Branche `lot-lme4-re-se`.

## Contexte

Depuis juillet, l'erreur standard des lignes d'effets aléatoires de `lmer` et `glmer` venait de merDeriv 0.2-6 (`vcov.lmerMod()` et `vcov.glmerMod()` avec `full = TRUE, ranpar = "var"`). Mesurée le 2026-11-19 contre glmmTMB sur les mêmes modèles, l'erreur standard d'un `glmer` était fausse :

| Modèle | Ligne | merDeriv | Hessienne | glmmTMB |
|:--|:--|--:|--:|--:|
| `cbpp`, logit, `(1 \| herd)` | σ herd | 0,2228 | 0,1786 | 0,1786 |
| binaire simulé, `(1 \| g)` | σ g | 0,1781 | 0,1592 | 0,1592 |
| Poisson (`grouseticks`) | σ LOCATION | 0,1095 | 0,1078 | 0,1078 |
| logit simulé, `(x \| g)` | σ x | 0,3517 | 0,1345 | 0,1345 |

Trois causes dans merDeriv. `vcov.glmerMod(ranpar = "var")` applique le facteur `(1/2) * sd^(-1/2)` à la place de la dérivée `1/(2 sd)`. Il suit l'ordre des paramètres de `devfun2()`, qui range sous lme4 2.x les écarts-types avant les corrélations, alors que ses noms de colonnes et spicy suivent le triangle inférieur. Sa jacobienne des corrélations est diagonale et de signe faux. Les lignes d'un `lmer` étaient justes. Mais `vcov.lmerMod()` ignore les poids et masque la méthode de lme4 une fois merDeriv chargé. Son coût croît comme n puissance 3,4 (63 s à n = 2 000), d'où le plafond `options(spicy.re_se_max_n = )`, 1 000 par défaut, au-delà duquel le tableau n'avait plus d'erreur standard.

Les signalements amont sont dans `dev/merderiv_glmer_ranpar_var_draft.md` (dépôt principal, hors suivi git).

## Décision

L'erreur standard des lignes d'effets aléatoires d'un `lmer` et d'un `glmer` est l'inverse de la moitié de la hessienne de la déviance, paramétrée en (écarts-types, corrélations) par terme aléatoire, plus σ pour un `lmer`. C'est l'information observée, celle de nlme et de glmmTMB. Le code est dans `.merMod_re_sdcor_vcov()` (`R/regression_frame_merMod.R`).

- `lmer` : `getME(fit, "devfun")` est la déviance profilée en σ. Elle est dé-profilée comme le fait `devfun2()` de lme4, d(θ, σ) = ldL2 [+ ldRX2] + pwrss/σ² + ν log(2πσ²), avec ν = n − p pour REML et n pour ML. La même formule vaut en REML et en ML, et les poids passent par pwrss. La fonction de déviance d'un `lmer` partage les objets de référence de l'ajustement (`pp`, `resp`). L'évaluer loin de l'optimum déplaçait l'ajustement lui-même (mesuré : `getME(fit, "theta")` et `REMLcrit(fit)` changent). Elle est donc construite sur des copies (`fit@pp$copy()`, `fit@resp$copy()`).
- `glmer` : la déviance de Laplace ou de quadrature en (θ, β). Quand lme4 a stocké sa hessienne à l'optimum (`fit@optinfo$derivs$Hessian`, différences centrées à pas absolu), elle est reprise et ramenée en (écart-type, corrélation) par la jacobienne de θ. lme4 ne la stocke pas au-delà de `check.conv.nobsmax` (10 000 observations) ni sous `calc.derivs = FALSE`. La hessienne est alors calculée avec un pas absolu (voir plus bas). Avec `nAGQ = 0`, la déviance ne prend que θ et la hessienne porte sur les seuls écarts-types et corrélations.
- Pas numérique : extrapolation de Richardson de numDeriv à deux tours, depuis 1e-3 d'une échelle propre à chaque paramètre. L'échelle vaut le plus grand de |écart-type| et σ pour un écart-type de `lmer`, 1 pour une corrélation et pour un écart-type de `glmer`, le plus grand de |β| et de son erreur standard pour un effet fixe. Le pas relatif de numDeriv (1e-4 du paramètre) se noie dans le bruit de PIRLS quand un paramètre est proche de 0. Il déplace aussi la sixième décimale de l'erreur standard de `sleepstudy` en `lmer`.
- Une matrice qui n'est pas définie positive, une déviance qui ne retrouve pas le critère de l'ajustement à 1e-6 près, un ajustement singulier : les cellules restent vides, comme aujourd'hui. Un `glmer` binomial négatif (dispersion tenue fixe par lme4), un `glmer` à paramètre d'échelle et une structure de covariance de lme4 2.x autre que la structure non contrainte gardent aussi leurs lignes sans erreur standard. merDeriv refusait déjà les deux premiers : ce n'est pas une perte. Sur un `glmer` Gamma simulé (20 groupes, σ = 0,387), le tableau n'avait déjà ni erreur standard ni intervalle, et glmmTMB donne 0,0688 sur le même modèle. Sur un binomial négatif (σ = 0,501), glmmTMB donne 0,0969.
- Des cellules vides ont leur raison sous le tableau, sur le modèle de `note_re_apvar_not_pd` de `lme` (même place, une ligne commune ou une ligne par modèle touché). Trois clés, en anglais et en français : `note_re_se_unavailable_dispersion` (« the model has an estimated dispersion parameter »), `note_re_se_unavailable_hessian` (« the Hessian of the deviance is not positive definite »), `note_re_se_unavailable_structure` (« they are not computed for this covariance structure »). Avant ce lot, ces cellules restaient vides sans explication. Un ajustement singulier garde sa propre note. Sous `re_ci = "profile"`, un binomial négatif reçoit l'intervalle de profil de lme4 et la note ne sort pas. lme4 refuse de profiler un `glmer` à paramètre d'échelle estimé (« can't (yet) profile GLMMs with non-fixed scale parameters ») : les cellules restent vides et un avertissement le dit.
- L'intervalle de Wald d'un écart-type reste sur log σ (relevé `2026-11-19-re-se-delta.md`). Celui d'une corrélation passe sur atanh ρ, tanh(atanh ρ ± z·SE/(1 − ρ²)), comme pour `lme`. Il reste dans (−1, 1) sans bornage.
- merDeriv n'est plus appelé. Le plafond `spicy.re_se_max_n`, son avertissement et la note `note_re_se_skipped` disparaissent, faute d'objet (voir les temps).
- Sous `re_ci = "profile"`, spicy garde l'intervalle de lme4. lme4 profile la déviance ML même pour un ajustement REML : `devfun2()` appelle `refitML()` (lme4 2.1-0, `R/profile.R`, ligne 511, appelé par `profile.merMod()` ligne 76). La note le dit désormais pour un ajustement REML : « For a REML fit, the profile is that of the ML criterion. »

## Ce qui a été mesuré

Accord avec glmmTMB, les deux optimiseurs resserrés (bobyqa `rhoend = 1e-12` et `tolPwrss = 1e-12` pour lme4, nlminb `rel.tol = 1e-14` pour glmmTMB). glmmTMB estime log σ et calcule sa hessienne par différentiation automatique. L'écart relatif maximal par modèle sur l'erreur standard des écarts-types, des corrélations et de σ :

| Modèle | Écart max. | Écart des estimations |
|:--|--:|--:|
| `sleepstudy`, ordonnée, REML | 8,9e-7 | 5,5e-7 |
| `sleepstudy`, ordonnée, ML | 2,8e-7 | 2,1e-7 |
| `sleepstudy`, pente, REML | 4,2e-6 | 8,8e-5 |
| `sleepstudy`, pente, ML | 3,1e-5 | 2,5e-5 |
| `cbpp` | 6,1e-7 | 3,0e-7 |
| binaire simulé | 1,6e-6 | 9,0e-7 |
| Poisson simulé | 3,2e-8 | 1,2e-7 |
| logit à pente aléatoire | 4,4e-6 | 3,4e-5 |
| trois termes aléatoires | 2,2e-6 | 1,1e-6 |
| deux facteurs croisés (`Penicillin`) | 1,8e-6 | 3,0e-8 |

Sur `sleepstudy` en ML, l'écart vient de l'optimum de glmmTMB, qui s'arrête à 2,5e-5 de celui de lme4 (« singular convergence »). Contre une vraisemblance gaussienne dense écrite à partir de Z, X, y et des poids, et dérivée indépendamment de lme4, l'écart tombe sous 1,0e-6 sur les quatre modèles de `sleepstudy`, sous 1,3e-6 sur trois termes et sur `Penicillin`.

`lmer` pondéré (`sleepstudy`, poids uniformes sur [0,5 ; 2]) : écart maximal 4,3e-7 à la vraisemblance dense. glmmTMB ne sert pas d'oracle ici, ses poids gaussiens multiplient la log-vraisemblance et ne sont pas des poids de précision.

Valeurs de référence, ajustements par défaut : 6,811906 et 1,727074 (`sleepstudy`, ordonnée, REML) ; 5,8363, 0,3250, 1,2480 et 1,5080 (pente, REML) ; 0,17856 (`cbpp`, contre 0,2228 avant). `cbpp` avec `nAGQ = 10` : 0,18052, la valeur de l'inverse de la hessienne de `devfun2()`.

Paramètres proches de 0. Une corrélation estimée à 0 s'obtient en décalant la covariable (les estimations sont équivariantes), une ordonnée à l'origine à 0 par un décalage (`offset`). Écart relatif à glmmTMB :

| Modèle | Pas relatif (piège) | Hessienne stockée | Pas absolu |
|:--|--:|--:|--:|
| logit `(x \| g)`, ρ = 5e-8, β0 = 1e-7 | 5,5e-4 | 1,1e-5 | 1,4e-5 |
| logit `(x \| g)`, β0 = −8e-5 (par hasard) | 9,5e-4 | 1,1e-5 | 6,0e-6 |
| `sleepstudy`, ρ = −3e-6 | sans objet | sans objet | 2,7e-6 |

Un Poisson simulé dont l'écart-type est estimé à 0,0046 donne 0,6300 contre 0,5984 pour glmmTMB. L'écart vient des deux optima dans une région très plate. Évaluée à l'optimum de lme4 (`TMB::sdreport(par.fixed = )`), la hessienne de glmmTMB donne 0,6302, à 3e-4.

Temps du calcul de l'erreur standard (`.merMod_re_sdcor_vcov()`), sous Windows, un cœur :

| Modèle | n = 2 000 | n = 20 000 | n = 200 000 |
|:--|--:|--:|--:|
| `lmer`, `(1 \| g)` | 0,39 s | 0,16 s | 1,9 s |
| `lmer`, `(x \| g)` | 0,11 s | 0,73 s | 5,8 s |
| `glmer` logit, `(1 \| g)` | 0,04 s | 4,6 s | 52 s |
| `glmer` logit, `(x \| g)` | 0,00 s | 14,9 s | non mesuré |

Groupes de 20 observations. Les ajustements `glmer` prennent 2, 19 et 238 s pour `(1 | g)`, 6 et 59 s pour `(x | g)`. À n = 2 000, lme4 a stocké sa hessienne et le calcul est immédiat. Au-delà de 10 000 observations, le pas absolu coûte environ un quart du temps de l'ajustement. Un `lmer` reste sous 6 s à 200 000 observations. Le plafond n'a plus d'objet et il est retiré. merDeriv prenait 63 s à n = 2 000.

Avant/après sur les ajustements des tests existants. Les lignes `lmer` bougent de 3e-5 au plus sur `sleepstudy`, `Dyestuff` et `Penicillin`, et jusqu'à 2,35 % sur une corrélation d'un modèle à trois termes en ML (1,1 % sur un écart-type) : merDeriv prenait l'information attendue, la hessienne est l'information observée. Les lignes `glmer` bougent de 20 % (`cbpp`) à 63 % (écart-type d'une pente).

Intervalle d'une corrélation. Somme des écarts des deux bornes au profil de lme4, sur des ajustements ML (le profil de lme4 est celui du critère ML) :

| Modèle | ρ | ρ ± z·SE (borné) | atanh | Profil | Écart linéaire | Écart atanh |
|:--|--:|:--|:--|:--|--:|--:|
| `sleepstudy`, pente, ML | 0,081 | −0,553 à 0,715 | −0,506 à 0,617 | −0,482 à 0,685 | 0,102 | 0,092 |
| `Orthodont`, `(age \| Subject)`, ML | −0,582 | −1 à 0,142 | −0,942 à 0,405 | −1 à 1 | 0,858 | 0,653 |
| logit simulé, `(x \| g)` | 0,274 | −0,253 à 0,801 | −0,281 à 0,691 | −0,268 à 0,713 | 0,103 | 0,034 |
| trois termes, ML, (Intercept)–x1 | 0,448 | 0,187 à 0,709 | 0,155 à 0,669 | 0,156 à 0,636 | 0,104 | 0,033 |
| trois termes, ML, (Intercept)–x2 | −0,368 | −0,659 à −0,077 | −0,618 à −0,050 | −0,571 à −0,058 | 0,108 | 0,055 |
| trois termes, ML, x1–x2 | 0,135 | −0,202 à 0,472 | −0,205 à 0,446 | −0,215 à 0,370 | 0,116 | 0,087 |
| logit simulé, ρ = 0,69 | 0,694 | −0,251 à 1 | −0,748 à 0,991 | −1 à 1 | 0,749 | 0,262 |
| `lmer` simulé, ρ = 0,5, ML | 0,615 | 0,256 à 0,974 | 0,138 à 0,860 | 0,190 à 0,907 | 0,132 | 0,099 |

L'intervalle sur atanh est plus proche du profil sur les huit corrélations, pour une somme des écarts de 1,32 contre 2,27. Il ne touche jamais ±1 sans bornage. La question laissée ouverte par le relevé `2026-11-19-re-se-delta.md` (deux modèles réels, résultat partagé) est tranchée sur cette batterie.

## Limites

- Une erreur standard de Wald près du bord reste une approximation. L'intervalle sur log σ ne descend pas sous 0, et `re_ci = "profile"` reste la voie qui respecte le bord.
- Un `glmer` au-delà de 10 000 observations n'a pas de hessienne stockée. Le pas absolu coûte alors une vingtaine à une cinquantaine d'évaluations de la déviance, soit une part du temps de l'ajustement (voir les temps).
- Le profil d'un ajustement REML reste celui du critère ML. Un profil REML maison serait une fonctionnalité. Il faudrait profiler `d(θ, σ)` ci-dessus, paramètre par paramètre, en réoptimisant les autres à chaque point, avec une recherche de racine de part et d'autre et le traitement du bord à 0 et à ±1. C'est le travail de `profile.merMod()` (environ 600 lignes dans lme4) à refaire et à entretenir. L'étude du 2026-11-19 en a écrit une version de 30 lignes (`my_profile()` de `01_compare.R`), sans traitement du bord ni des échecs d'optimisation. Elle coûte de 0,2 à 10 s par modèle sur les exemples mesurés.
- Un `glmer` binomial négatif, un `glmer` à paramètre d'échelle et une structure de covariance structurée de lme4 2.x restent sans erreur standard.

## Le plus petit correctif, écarté

Reprendre la hessienne stockée pour `glmer` seulement, et garder merDeriv pour `lmer`. Le correctif tiendrait en une quarantaine de lignes. Il est écarté pour quatre raisons mesurées. merDeriv reste chargé et masque `vcov()` de lme4. `vcov.lmerMod()` ignore les poids. Son coût impose le plafond, et le tableau d'un `lmer` de plus de 1 000 observations reste sans erreur standard. `glmer` sans hessienne stockée (n > 10 000) n'aurait aucune voie. La hessienne de la déviance couvre les deux classes avec le même code, prend les poids, et coûte moins d'une seconde pour un `lmer` de 20 000 observations.

## Garde

`tests/testthat/test-lme4_re_se_hessian.R` : valeurs épinglées, accord à 1e-5 avec glmmTMB sur les modèles du tableau, `lmer` pondéré contre la vraisemblance dense, paramètres proches de 0 (hessienne stockée et pas absolu), `nAGQ = 0` et `nAGQ = 10`, ajustement intact après le tableau, intervalle atanh, binomial négatif sans erreur standard, note du profil REML.
