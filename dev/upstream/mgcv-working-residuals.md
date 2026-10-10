# mgcv : residuals(type = "working") de bam() et des gam à lien non canonique

- **Cible** : mgcv 1.9-4 (CRAN), R 4.6.1
- **Canal** : courriel au mainteneur, Simon Wood <simon.wood@r-project.org>
  (champ Maintainer de la DESCRIPTION ; pas de BugReports ni de tracker
  public)
- **Envoyé** : 2026-10-10, par Amal depuis sa boîte, texte ci-dessous tel quel
- **Statut** : envoyé, sans réponse
- **Côté spicy** : jusqu'à la 0.13.0, HC* et CR* des gam et bam passaient
  par sandwich, qui lit `residuals(x, "working")` et `weights(x, "working")` :
  valeurs fausses pour bam non gaussien et pour gam à lien non canonique
  (registre n°352). Depuis 6002f2d6 (2026-10-10), spicy forme le sandwich
  lui-même à partir du score GLM (R/vcov_mgcv.R) et ne lit plus ces
  vecteurs : la réponse de mgcv ne change rien au code de spicy.
- **Faits établis** : bam.r 2770-2771 range les résidus de déviance dans
  $residuals (famille sans fonction residuals propre) et 2774 en tire la
  déviance ; gam.fit3.r 118 passe en Newton complet pour un lien non
  canonique, 507-512 et 811 stockent (y − μ)/(mu.eta·α) avec
  α = 1 + (y − μ)(V'/V + g''·mu.eta), 823-824 renvoient les poids de
  Fisher dans $weights ; mgcv.r 3441 : residuals.gam(type = "working")
  renvoie $residuals ; gamObject.Rd 143 : « the working residuals ».
- **Suggestion faite** : une phrase de doc si c'est la définition voulue ;
  sinon, calculer (y − μ)/mu.eta(η) à la volée dans residuals.gam(), comme
  les autres types, sans toucher $residuals ni la déviance (mgcv n'appelle
  residuals(type = "working") nulle part dans son code R).
- **Vérifications** : lignes lues dans le tarball CRAN ; class(bam) =
  bam, gam, glm, lm ; gam.fit3() tracé comme fitter des modèles du
  courriel ; bloc de code exécuté tel qu'écrit (scratchpad
  email_mgcv_short.R), chaque nombre du texte vient de cette sortie ;
  formule de Newton vérifiée à 2e-7 ou mieux sur six liens non canoniques
  et avec un terme lisse (mgcv_newton_check.R). Quatre relectures ; les
  deux premières versions citaient des nombres d'un autre jeu de données
  et une interprétation fausse (« dérivée aux valeurs de départ »).

---

```
Subject: mgcv 1.9-4: residuals(type = "working") of bam() and of gam() with a non-canonical link differ from the GLM working residuals

Dear Professor Wood,

Two observations on mgcv 1.9-4, made while computing sandwich variances from gam and bam fits: sandwich's estfun.glm(), which these objects inherit, forms the scores from residuals(x, "working") and weights(x, "working") as for a glm. The code is at the end (n = 400, no smooth terms, so that glm() gives the same coefficients).

1. For a family without its own residuals function, bam() stores the deviance residuals in $residuals (R/bam.r, lines 2770-2771), while ?gamObject documents residuals as "the working residuals for the fitted model" and residuals.gam(type = "working") returns that vector. For a binomial bam, residuals(b, "working") is identical to residuals(b, "deviance"), and differs by up to 3.0 from residuals(g, "working") of the gam() fit of the same model, which are (y - mu) / mu.eta(eta). Line 2774 computes the deviance as sum(object$residuals^2).

2. For gam() with a non-canonical link, gam.fit3() uses full Newton rather than Fisher scoring (R/gam.fit3.r, line 118) and stores the residual of the Newton pseudodata (lines 507-512 and 811): (y - mu) / (mu.eta * alpha) with alpha = 1 + (y - mu) * (dvar(mu) / var(mu) + d2link(mu) * mu.eta). The returned $weights are the Fisher weights (lines 823-824), so residuals * weights is the GLM score divided by alpha. For a canonical link gam.fit3() uses Fisher scoring and the stored residual is (y - mu) / mu.eta, as in glm(). For Gamma(link = "log"), alpha = y / mu and the stored vector is (y - mu) / y: on the data below it differs from glm()'s working residuals by up to 30, with the same coefficients to 2e-6.

Is this the intended meaning of residuals(type = "working")? If so, a sentence in ?gamObject and ?residuals.gam would help the packages that read these residuals as GLM working residuals. If not, one possibility that leaves $residuals and the deviance untouched: residuals.gam(type = "working") could compute (y - mu) / mu.eta(eta) from y, fitted.values and linear.predictors, as it does for the other types from y and fitted.values.

set.seed(7)
n <- 400
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
d$yb <- rbinom(n, 1, plogis(0.4 * d$x1))
d$yg <- rgamma(n, shape = 2, rate = 2 / exp(0.3 + 0.2 * d$x1))

# 1. bam(): working residuals are the deviance residuals
b <- mgcv::bam(yb ~ x1 + x2, family = binomial, data = d)
g <- mgcv::gam(yb ~ x1 + x2, family = binomial, data = d)
max(abs(coef(b) - coef(g)))  # 3.1e-13
identical(residuals(b, "working"), residuals(b, "deviance"))  # TRUE
max(abs(residuals(g, "working") - (d$yb - fitted(g)) / g$family$mu.eta(g$linear.predictors)))  # 2.4e-12
max(abs(residuals(b, "working") - residuals(g, "working")))  # 3.0

# 2. gam() with a non-canonical link: the Newton residual (y - mu) / (mu.eta * alpha)
a <- mgcv::gam(yg ~ x1 + x2, family = Gamma(link = "log"), data = d)
m <- glm(yg ~ x1 + x2, Gamma(link = "log"), d)
max(abs(coef(a) - coef(m)))  # 1.6e-06
max(abs(residuals(a, "working") - residuals(m, "working")))  # 30
mu <- fitted(a)
max(abs(residuals(a, "working") - (d$yg - mu) / d$yg))  # 2.0e-07
max(abs(a$weights - mu^2 / a$family$variance(mu)))  # 0: the Fisher weights mu.eta^2 / V(mu), all 1 here; the Newton weights would be alpha
a <- mgcv::gam(yg ~ x1 + x2, family = Gamma, data = d)
m <- glm(yg ~ x1 + x2, Gamma, d)
max(abs(residuals(a, "working") - residuals(m, "working")))  # 4.1e-12: canonical link

This report was prepared with the help of an AI assistant. The source lines were checked in the mgcv 1.9-4 tarball, and the numbers come from running the code above on my machine.

With my thanks for mgcv,

Amal Tawfik
```

À la fermeture : noter la version de mgcv et la décision prise (doc ou
calcul à la volée) dans l'en-tête ; côté spicy, rien à retirer, le
sandwich maison ne dépend pas de ces résidus.
