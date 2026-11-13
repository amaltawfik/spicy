# sandwich : vcovHC() faux pour tout type HC sous des poids a priori nuls

- **Cible** : sandwich 3.1-3, R 4.6.2
- **Canal** : courriel à Achim Zeileis. Le suivi public existe (Codeberg zeileis/sandwich) et
  l'issue #7 du 2026-10-30 (acoppock, estimatr), ouverte et sans réponse au 2026-11-13, signale
  déjà ce défaut pour lm (HC0, HC2, vcovCL, meat). Le corps ci-dessous est antérieur à ce
  constat : à ne pas envoyer tel quel.
- **Envoyé** : non envoyé
- **Statut** : dossier prêt le 2026-11-12, corps relu mot à mot le 2026-11-13 (reproducteur court ajouté, trois phrases précisées). Reproduction en base R et sandwich
  seuls (repro.R, repro.log, docs_check.log, findings.txt, archivés avec la
  session du 12.11). Chaque affirmation repose sur une sortie capturée.
- **Côté spicy** : compute_model_vcov() refuse tout type HC pour un lm ou
  glm à poids a priori nuls (f940f77e, spicy_invalid_input) et dit de
  retirer les lignes et de réajuster.

---

*Corps factuel, à envoyer dans les mots d'Amal.*

# vcovHC() on lm/glm fits with zero prior weights (sandwich 3.1-3)

sandwich 3.1-3 (current on CRAN), R 4.6.2, Windows 11. I did not check the
development version on R-Forge.

## Reproducer

```r
library("sandwich")
d <- mtcars
d$w <- 1
d$w[c(3, 10, 20)] <- 0
fit   <- lm(mpg ~ wt + hp, data = d, weights = w)
refit <- lm(mpg ~ wt + hp, data = d[d$w > 0, ])
all.equal(coef(fit), coef(refit))
#> [1] TRUE
c(nobs = nobs(fit), estfun_rows = NROW(estfun(fit)),
  residuals = length(residuals(fit)), hatvalues = length(hatvalues(fit)))
#>        nobs estfun_rows   residuals   hatvalues
#>          29          32          32          29
sqrt(diag(vcovHC(fit, type = "HC0"))) / sqrt(diag(vcovHC(refit, type = "HC0")))
#> (Intercept)          wt          hp
#>     0.90625     0.90625     0.90625
sqrt(diag(vcovHC(fit, type = "HC3"))) / sqrt(diag(vcovHC(refit, type = "HC3")))
#> (Intercept)          wt          hp
#>   0.9470938   0.9119261   0.7198574
#> Warning message:
#> In residuals^2/(1 - diaghat)^2 :
#>   longer object length is not a multiple of shorter object length
```

n = 32, of which 29 rows have a nonzero weight.
glm(am ~ wt, family = binomial, weights = w) on the same data gives the same
four lengths and the same HC0 ratio.

## What happens

1. HC0 and HC1. bread.lm() and bread.glm() scale by
   sum(summary(fit)$df[1:2]) = 29
   (bread(fit) = 29 * solve(X'WX)), while sandwich() divides by
   NROW(estfun(fit)) = 32 and meatHC(fit, "HC0") is crossprod(estfun(fit)) / 32.
   So vcovHC(fit, "HC0") = (29/32)^2 * V, where V is both vcovHC(refit, "HC0")
   and the sandwich computed by hand on the weighted fit (bread = solve(X'WX),
   meat = sum of (w_i e_i)^2 x_i x_i'). The HC0 standard errors are
   29/32 = 0.90625 times the correct ones, for lm and for glm, with no warning.
   HC1 has no warning either: its covariance is 29(29 - p) / (32(32 - p)) times
   that of the refit, 0.8125 for the lm (SE ratio 0.9014) and 0.815625 for the
   glm (SE ratio 0.9031).

2. HC2, HC3, HC4, HC4m, HC5. lm.influence() drops zero-weight cases, so
   hatvalues(fit) has 29 elements against 32 residuals. The omega formula in
   meatHC() (residuals^2 / (1 - diaghat)^2 for HC3) recycles the hat values, and
   base R warns "longer object length is not a multiple of shorter object
   length". The SE ratio then varies across coefficients, so no scalar
   rescaling repairs the matrix. For the lm under HC4 the largest relative
   difference in a covariance cell, |V_fit - V_refit| / |V_refit|, is 4.2.

3. type = "const" also differs from the refit, with no warning (SE ratios
   0.924, 0.880, 0.869 for the lm).

## Documentation

?vcovHC, ?sandwich, ?meat, ?bread and ?estfun have no sentence containing
"zero" or "weight". ?lm.influence says "Note that cases with weights == 0 are
dropped"; ?lm.summaries says of the residuals "In all these, zero weight cases
are never omitted". The two conventions meet in meatHC().

## Numbers

HC0 standard errors, zero-weight fit / refit (ratio 0.90625 everywhere):
lm  (Intercept) 1.675628 / 1.848969, wt 0.5452059 / 0.6016065, hp 0.006085241 / 0.006714748
glm (Intercept) 4.557030 / 5.028446, wt 1.424305 / 1.571647

HC3 standard errors, zero-weight fit / refit:
lm  (Intercept) 2.060051 / 2.175129, wt 0.6907419 / 0.7574538, hp 0.006747945 / 0.009374003
glm (Intercept) 4.918682 / 5.699328, wt 1.543135 / 1.771190

The attached repro.R (base R and sandwich only) prints every type for both
models, with the hand-computed HC0; repro.log is its output.

This report was prepared with the help of an AI assistant. The source
statements were checked in the installed sandwich 3.1-3, and the numbers come
from running the code above and the attached script on my machine.
