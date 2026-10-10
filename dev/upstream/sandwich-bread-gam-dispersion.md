# sandwich : bread.gam() omet la dispersion (sandwich d'un gam déflaté de φ²)

- **Cible** : sandwich 3.1-3 (CRAN), mgcv 1.9-4, R 4.6.1
- **Canal** : courriel au mainteneur, Achim Zeileis <Achim.Zeileis@R-project.org>
  (champ Maintainer de la DESCRIPTION), plutôt que l'issue Codeberg
  (BugReports) : Codeberg restreint les contenus produits par LLM (billet
  du 2026-07-23) ; le courriel le dit en une phrase de transparence.
- **Envoyé** : 2026-10-10, par Amal depuis sa boîte, texte ci-dessous tel quel
- **Statut** : envoyé, sans réponse
- **Côté spicy** : jusqu'à la 0.13.0, HC* et CR* d'un gam passaient par
  sandwich et héritaient du défaut (SE divisés par φ pour les familles à
  dispersion libre ; registre n°351-352). Depuis 6002f2d6 (2026-10-10,
  lot « sandwich propre pour mgcv »), spicy forme le sandwich lui-même
  (R/vcov_mgcv.R, classe interne spicy_mgcv_sandwich) et un test épingle
  le défaut de sandwich : il échouera le jour où bread.gam() est corrigé,
  et c'est le signal pour relâcher ce test. Le second défaut, les résidus
  de travail de mgcv (bam et liens non canoniques), a son propre
  brouillon, dev/mgcv_residuals_draft.md, non envoyé.
- **Vérifications, toutes refaites le 2026-10-10** : lignes 60-65 lues
  dans le tarball CRAN ; absence d'estfun.gam vérifiée dans le NAMESPACE
  de sandwich et par getS3method avec mgcv chargé ; liste des fonctions
  qui appellent sandwich() établie sur la source (vcovHC, vcovCL, vcovHAC,
  vcovPL, vcovPC ; vcovOPG, vcovBS et vcovJK n'utilisent pas bread(), ce
  que le courriel ne dit pas) ; le bloc de code exécuté tel qu'écrit avec
  le sandwich CRAN puis avec le patch appliqué à la source CRAN et
  installé dans une bibliothèque temporaire ; chaque nombre du texte vient
  de ces deux sorties, à trois chiffres significatifs.

---

```
Subject: sandwich 3.1-3: bread.gam() omits the dispersion, patch tested

Dear Professor Zeileis,

In sandwich 3.1-3, bread.gam() (R/bread.R, lines 60-65) returns sx$cov.unscaled * sx$n with sx <- summary(x), without the dispersion. A gam object has class c("gam", "glm", "lm") and sandwich has no estfun.gam(), so estfun() dispatches to estfun.glm(), which divides by the dispersion unless the family is poisson, binomial or Negative Binomial. bread.glm() multiplies by that same dispersion and the two cancel in sandwich(). bread.gam() does not multiply by it, so for the other families (Gaussian and Gamma among them) every variance estimate that sandwich() assembles (vcovHC(), vcovCL(), vcovHAC(), vcovPL(), vcovPC()) is too small by the dispersion squared, and the standard errors by the dispersion.

library(sandwich)
set.seed(7)
n <- 400
d <- data.frame(g = factor(rep(1:20, each = 20)), x1 = rnorm(n), x2 = rnorm(n))
d$y  <- 1 + 0.5 * d$x1 - 0.3 * d$x2 + rnorm(n, sd = 3) + rnorm(20, sd = 2)[d$g]
d$yb <- rbinom(n, 1, plogis(0.4 * d$x1 - 0.5 * d$x2 + rnorm(20, sd = 1)[d$g]))
d$yg <- rgamma(n, shape = 2, rate = 2 / exp(0.3 + 0.2 * d$x1))
m_lm  <- lm(y ~ x1 + x2, d)
m_glm <- glm(y ~ x1 + x2, gaussian, d)
m_gam <- mgcv::gam(y ~ x1 + x2, data = d)
max(abs(coef(m_glm) - coef(m_gam)))  # 1.6e-15
se <- function(V) sqrt(diag(V))[["x1"]]
se(vcovCL(m_lm, cluster = d$g))   # 0.0982
se(vcovCL(m_glm, cluster = d$g))  # 0.0979
se(vcovCL(m_gam, cluster = d$g))  # 0.00786
se(vcovHC(m_lm, "HC1"))   # 0.163
se(vcovHC(m_glm, "HC1"))  # 0.163
se(vcovHC(m_gam, "HC1"))  # 0.0131
meat(m_glm)[2, 2]   # 0.0688
meat(m_gam)[2, 2]   # 0.0688
bread(m_glm)[2, 2]  # 12.4
bread(m_gam)[2, 2]  # 0.993

The glm and the gam have the same coefficients and use the same estfun() method, so their meats are identical. Their breads differ by the dispersion (12.5 here), so the sandwich of the gam is the glm's divided by the dispersion squared, and its standard errors are the glm's divided by the dispersion (0.0979 / 0.00786 = 12.5).

A fix: give bread.gam() the dispersion of bread.glm().

bread.gam <- function(x, ...)
{
  if(!is.null(x$na.action)) class(x$na.action) <- "omit"
  sx <- summary(x)
  wres <- as.vector(residuals(x, "working")) * weights(x, "working")
  dispersion <- if(substr(x$family$family, 1L, 17L) %in% c("poisson", "binomial", "Negative Binomial")) 1
    else sum(wres^2)/sum(weights(x, "working"))
  sx$cov.unscaled * sx$n * dispersion
}

With this patch applied to the CRAN source and installed, on the data above:

se(vcovCL(m_gam, cluster = d$g))  # 0.0979, the glm's value
se(vcovHC(m_gam, "HC1"))  # 0.163, the value of the lm and the glm
b_glm <- glm(yb ~ x1 + x2, binomial, d)
b_gam <- mgcv::gam(yb ~ x1 + x2, family = binomial, data = d)
se(vcovCL(b_glm, cluster = d$g))  # 0.106
se(vcovCL(b_gam, cluster = d$g))  # 0.106, before and after the patch
g_glm <- glm(yg ~ x1 + x2, Gamma, d)
g_gam <- mgcv::gam(yg ~ x1 + x2, family = Gamma, data = d)
se(vcovCL(g_glm, cluster = d$g))  # 0.017
se(vcovCL(g_gam, cluster = d$g))  # 0.017 (0.0362 before the patch)

The patch corrects the bread only. On the data above, a Gamma(link = "log") gam has the glm's coefficients, but its residuals(x, "working"), which estfun.glm() reads, differ from the glm's, and the patched vcovCL() gives 0.227 against 0.0293 for the glm (0.028 before the patch, the two effects offsetting each other). Those residuals come from mgcv.

l_glm <- glm(yg ~ x1 + x2, Gamma(link = "log"), d)
l_gam <- mgcv::gam(yg ~ x1 + x2, family = Gamma(link = "log"), data = d)
max(abs(coef(l_glm) - coef(l_gam)))  # 1.6e-6
max(abs(residuals(l_glm, "working") - residuals(l_gam, "working")))  # 33
se(vcovCL(l_glm, cluster = d$g))  # 0.0293
se(vcovCL(l_gam, cluster = d$g))  # 0.227 (0.028 before the patch)

This report was prepared with the help of an AI assistant. The source lines were checked in the sandwich 3.1-3 tarball, and the numbers come from running the code above on my machine, with the CRAN build and with the patched build.

With my thanks for sandwich,

Amal Tawfik
```

À la fermeture : noter la version de sandwich corrigée dans l'en-tête ;
côté spicy, le test qui épingle le défaut (tests/testthat/test-vcov_mgcv.R)
échouera et devra être retiré dans le même commit ; le sandwich maison
reste nécessaire tant que les résidus de travail de mgcv ne sont pas
ceux d'un GLM pour les liens non canoniques.
