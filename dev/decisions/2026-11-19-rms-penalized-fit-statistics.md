# ADR 2026-11-19 — Statistiques d'ajustement d'un modèle rms pénalisé

Statut : décidé le 2026-11-19, sur délégation d'Amal pour les décisions techniques. Mis en œuvre le même jour (branche `lot-rms-penalized-aic`).

## Contexte

Mesure faite avec R 4.6.2 et rms 8.2-1, sur `pima <- rbind(MASS::Pima.tr, MASS::Pima.te)` (532 observations) :

```r
f_mle <- lrm(type ~ rcs(age, 4) + bmi + glu + npreg + ped, data = pima, x = TRUE, y = TRUE)
f_pen <- update(f_mle, penalty = 2)
```

Pour le modèle pénalisé, `f_pen$stats["d.f."]` vaut 5,600312 (degrés de liberté effectifs) et `f_pen$stats["Model L.R."]` vaut 220,8599. `f_pen$deviance` vaut c(676,788, 462,2254).

`logLik(f_pen)` renvoie -231,1127 avec `df` 6,600312, et `AIC(f_pen)` renvoie 475,426. C'est la log-vraisemblance pénalisée : 462,2254 = -2 × -231,1127. La log-vraisemblance sans le terme de pénalité, aux estimations pénalisées, vaut -227,9641. On l'obtient à la main sur `plogis(f_pen$linear.predictors)`. Elle vaut aussi -(676,788 - 220,8599) / 2.

`pentrace()` choisit la pénalité sur le critère « Model L.R. - 2 × d.f. effectifs ». Il vaut 209,66 pour la pénalité 2 et 208,57 pour le maximum de vraisemblance, donc le modèle pénalisé est le meilleur. `AIC(f_pen, type = "chisq")` donne aussi 209,6593.

spicy affichait jusqu'ici un AIC de 470,2 pour le maximum de vraisemblance et de 475,4 pour le modèle pénalisé. Le tableau classait les deux modèles à l'inverse du critère qui avait choisi la pénalité. L'AIC de rms compte la pénalité deux fois : une fois dans la vraisemblance, une fois dans les degrés de liberté effectifs.

## Décision

Pour un `lrm` pénalisé, à issue binaire ou ordinale, les statistiques d'ajustement bâties sur la log-vraisemblance emploient la log-vraisemblance sans le terme de pénalité aux estimations pénalisées. Le nombre de paramètres est celui que rms donne déjà à `logLik()` : les degrés de liberté effectifs plus le nombre de constantes. Les lignes concernées sont `log_lik`, `deviance`, `aic`, `aicc`, `bic` et `pseudo_r2_mcfadden`. La valeur vient de -(déviance nulle - Model L.R.) / 2 (`.lrm_loglik()`, `R/regression_frame_rms.R`).

Le R² de Nagelkerke vient de `fit$stats["R2"]`, que rms calcule déjà sans la pénalité. Il ne change pas. Un `lrm` non pénalisé ne change pas non plus.

Un `ols` pénalisé suit déjà cette règle. `logLik.ols` applique la vraisemblance gaussienne de `lm` aux résidus du modèle pénalisé, sans terme de pénalité, avec les degrés de liberté effectifs plus deux (constante et sigma). Un test le fige.

Quand au moins une de ces lignes est affichée pour un modèle pénalisé, la note ajoute après « Penalized estimates (penalty: 2). » la phrase « Fit statistics of the penalized model: log-likelihood without the penalty term, effective degrees of freedom. » (clé `note_penalized_fit_stats`). Dans un tableau à plusieurs modèles, elle porte l'étiquette du modèle comme la phrase voisine. Elle reste dans un tableau qui ne montre que les statistiques d'ajustement.

Avec la règle, l'AIC du modèle pénalisé vaut 469,1 contre 470,2 pour le maximum de vraisemblance. Sur une grille de pénalités, l'AIC de spicy plus le critère `aic` de `pentrace()` est constant (déviance nulle + 2), donc les deux classent les modèles dans le même ordre.

## Source

Harrell F. E. (2015), Regression Modeling Strategies, 2e éd., Springer, p. 211, éq. 9.65 : « LR χ² − 2 × effective d.f., where LR χ² is the likelihood ratio χ² for the penalized model, but ignoring the penalty function ».

mgcv suit la même convention pour un GAM : `logLik.gam` renvoie la vraisemblance sans pénalité avec les degrés de liberté effectifs.

## Alternatives écartées

- Suivre `AIC(fit)` de rms. Il mélange une vraisemblance pénalisée et des degrés de liberté effectifs et il classe les modèles à l'inverse de `pentrace()` et de `AIC(fit, type = "chisq")` du même paquet.
- Refuser ces lignes pour un modèle pénalisé. La statistique existe et a une source. Le refus priverait l'utilisateur de la comparaison qui justifie la pénalité.

## Garde

`tests/testthat/test-rms_penalized_fit_stats.R` : vraisemblance binomiale recalculée à la main, AIC égal à 676,788 - LR + 2 × (d.f. effectifs + constantes), ordre des AIC inverse de celui de `pentrace()` sur une grille de pénalités, `lrm` ordinal pénalisé (probabilité de la catégorie observée par `predict(fit, type = "fitted.ind")`), `ols` pénalisé, phrase de la note en anglais et en français.
