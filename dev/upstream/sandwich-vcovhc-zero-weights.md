# sandwich : vcovHC() faux pour tout type HC sous des poids a priori nuls

- **Cible** : sandwich 3.1-3, R 4.6.2
- **Canal** : courriel à Achim Zeileis (pas de suivi d'issues public)
- **Envoyé** : non envoyé
- **Statut** : dossier prêt le 2026-11-12. Reproduction en base R et sandwich
  seuls (repro.R, repro.log, docs_check.log, findings.txt, archivés avec la
  session du 12.11). Chaque affirmation repose sur une sortie capturée.
- **Côté spicy** : compute_model_vcov() refuse tout type HC pour un lm ou
  glm à poids a priori nuls (f940f77e, spicy_invalid_input) et dit de
  retirer les lignes et de réajuster.

---

*Corps factuel, à envoyer dans les mots d'Amal.*

# vcovHC() on lm/glm fits with zero prior weights (sandwich 3.1-3)

## Setup

mtcars, w = 1 except w = 0 on rows 3, 10 and 20 (n = 32, n_nonzero = 29).
fit <- lm(mpg ~ wt + hp, data = mtcars, weights = w); refit = the same model on
the 29 rows with w > 0 (same coefficients). Same with glm(am ~ wt, binomial).

## What happens

1. HC0 and HC1. bread(fit) is scaled by nobs(fit) = 29 (bread = 29 * solve(X'WX)),
   while sandwich() divides by NROW(estfun(fit)) = 32 and meatHC(fit, "HC0") is
   crossprod(estfun(fit)) / 32. So vcovHC(fit, "HC0") = (29/32)^2 * V, where V is
   both vcovHC(refit, "HC0") and the hand-computed sandwich on the weighted fit
   (bread = solve(X'WX), meat = sum of (w_i e_i)^2 x_i x_i'). The HC0 standard
   errors are 29/32 = 0.90625 times the correct ones, for lm and for glm. No
   warning. HC1 carries a similar factor (0.8125 for lm, 0.815625 for glm).

2. HC2, HC3, HC4, HC4m, HC5. lm.influence() drops zero-weight cases, so
   hatvalues(fit) has 29 elements against 32 residuals. The omega formula
   (residuals^2 / (1 - diaghat)^k) recycles and raises "longer object length is
   not a multiple of shorter object length". The matrices are wrong and no
   rescaling fixes them: for lm, HC3 SEs are 2.060 / 0.691 / 0.00675 against
   2.175 / 0.757 / 0.00937 on the refit; HC4 differs by a factor up to 4.2 in
   a cell.

3. type = "const" differs from the refit too (no warning).

## Documentation

?vcovHC, ?sandwich, ?meat, ?bread and ?estfun say nothing about zero weights.
?lm.influence states that cases with weights == 0 are dropped; ?lm.summaries
states that zero weight cases are never omitted from residuals(). The two
conventions meet inside meatHC().

## Numbers

HC0 standard errors, zero-weight fit / refit (ratio 0.90625 everywhere):
lm  (Intercept) 1.675628 / 1.848969, wt 0.5452059 / 0.6016065, hp 0.006085241 / 0.006714748
glm (Intercept) 4.557030 / 5.028446, wt 1.424305 / 1.571647

HC3 standard errors, zero-weight fit / refit:
lm  (Intercept) 2.060051 / 2.175129, wt 0.6907419 / 0.7574538, hp 0.006747945 / 0.009374003
glm (Intercept) 4.918682 / 5.699328, wt 1.543135 / 1.771190

Full tables for HC2, HC4, HC4m, HC5, HC1 and const: findings.txt and repro.log.

## Minimal script

repro.R in this folder (Sys.setLanguage("en"), mtcars, sandwich only).
