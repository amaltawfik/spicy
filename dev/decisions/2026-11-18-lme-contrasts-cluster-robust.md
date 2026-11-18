# ADR 2026-11-18 — Variance robuste d'un lme ajusté avec ses propres contrastes : refus

**Statut** : décidé le 2026-11-18, sur délégation d'Amal pour les décisions techniques. Mis en œuvre le même jour.

## Contexte

Pour un `nlme::lme()`, clubSandwich reconstruit le plan par `model.matrix(formula(obj), data = droplevels(get_data(obj)))` (`R/lme.R`, lignes 89 à 92 de sa branche principale, commit 0e897f0), sans `contrasts.arg`. Les contrastes de la session s'appliquent donc, et `droplevels()` efface ceux qui sont posés sur les données. Quand l'ajustement a ses propres contrastes, la matrice de `vcovCR()` mêle le plan en contrastes de traitement et le « bread » du modèle recodé : elle n'est la variance d'aucun codage.

spicy imprimait ces erreurs standard. Sur un exemple simulé (30 grappes, facteur à trois niveaux, CR2) : 0,1024 au lieu de 0,1273 pour le premier contraste du facteur. Le défaut touche les trois façons de poser des contrastes : l'argument `contrasts`, `contrasts(d$A) <-` sur les données et `options(contrasts = )` changé depuis l'ajustement.

## Décision

`table_regression()` refuse la variance robuste d'un lme dont le plan diffère de celui que clubSandwich reconstruit (`.refuse_cr_nlme_contrasts()`, `R/vcov.R`), avec une erreur `spicy_invalid_input` qui dit de réajuster avec les contrastes par défaut ou de remettre le même `options(contrasts = )`.

Le refus, et non une correction : aucune transformation de la matrice de clubSandwich ne redonne la bonne (écart de 0,050 après la transformation inverse). La valeur juste demanderait un réajustement en contrastes de traitement.

## Ce qui a été vérifié

- La valeur juste, `M V_trt M'`, égale celle de l'ajustement `lmer` équivalent à 1e-13, pour lequel clubSandwich lit le plan dans l'ajustement.
- Un `gls` a le même défaut sous `options(contrasts = )`, mais spicy refuse déjà toute variance robuste pour cette classe.

## Suites

- Brouillon de signalement amont : `dev/clubsandwich_lme_contrasts_draft.md` (non posté, aucune issue existante sur ce point).
- Trouvé en validant le type III des modèles mixtes (ADR `2026-11-18-partial-type.md`).

## Garde

`tests/testthat/test-robust-vcov-mixed.R` : les trois façons de poser des contrastes et le cas inverse, avec l'oracle `lmer` à 1e-6 quand l'option est encore en vigueur.
