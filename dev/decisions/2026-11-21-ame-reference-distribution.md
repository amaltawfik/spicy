# ADR 2026-11-21 : la loi de référence du test d'un AME

**Statut** : décision d'Amal du 2026-11-21, mise en œuvre le même jour, branche `lot-ame-reference`.

## Mesure

Dans un modèle linéaire, l'AME d'un prédicteur est son coefficient. Le tableau leur donnait deux tests différents pour plusieurs classes. Sur `y ~ x + z`, n = 24 :

| Ajustement | Coefficient | p | AME avant | p |
|---|---|---|---|---|
| `lm()`, `glm()` gaussien, `feols()` | t(21) | .078 | t(21) | .078 |
| `rms::ols()`, `nlme::gls()`, `AER::ivreg()`, `lm_robust()` et `iv_robust()` classiques | t(21) | .078 | z | .063 |
| `lm_robust()` HC2 | t(21) | .038 | z | .027 |
| `nlme::lme()` | t(16) | .082 | z | .063 |

marginaleffects teste en z par défaut (`df = Inf`). spicy ne lui passait des degrés de liberté que pour `lm`, `glm`, fixest et les plans d'enquête.

## Règle

L'AME est testé sur la loi de référence des coefficients dont il dépend.

1. Les coefficients partagent un nombre fini de degrés de liberté : l'AME est testé en t sur ce nombre.
2. Les coefficients ont des degrés de liberté différents (containment de `lme`, Satterthwaite de lmerTest, CR2 d'estimatr avec grappes) : chaque AME prend le plus petit parmi les coefficients dont il dépend. Pour un terme linéaire, on retrouve le test du coefficient.
3. Les coefficients sont testés en z : l'AME aussi.

La règle se pose dans `.attach_ame_to_frame_coefs()` (`R/regression_ame.R`), par où passent les AME de toutes les classes à cadre. `.ame_reference_df()` lit le `df` et le type de test des lignes de coefficient du cadre, `Inf` pour un z. Une seule valeur est passée à `avg_slopes(df = )`. S'il y en a plusieurs, `.ame_rows_min_df()` recalcule p et intervalle de chaque ligne à partir de l'estimation, de l'erreur standard et du plus petit `df`. L'erreur standard ne change pas.

Les coefficients dont un AME dépend sont ceux des termes qui contiennent sa variable (`assign` de la matrice du modèle et matrice `factors` des termes), dans un modèle linéaire en ses coefficients. Ce sont les colonnes non nulles du jacobien. Le jacobien numérique d'`avg_slopes()` ne sert pas à les trouver : son bruit (jusqu'à 1e-3 sur une colonne nulle, mesuré sur un `lme`) dépasse une vraie petite charge (7,8e-4 sur `poly(x, 2)2`). Hors modèle linéaire, tous les coefficients comptent.

Le chemin de `lm` (`df = "residual"`) et celui de `glm` (`.glm_wald_df()`) suivaient déjà la règle et ne changent pas. Le `df` explicite de fixest disparaît : la règle donne le même.

## Sources

emmeans, vignette `models` (`system.file("doc", "models.Rmd", package = "emmeans")`, lignes 304 à 306, emmeans 2.0.4), vérifiée :

> `"containment"` mode (for `lme` models) determines the degrees of freedom for the coarsest grouping involved in the contrast or linear function involved, so it tends to under-estimate the degrees of freedom.

Stata, manuel de `margins` (`https://www.stata.com/manuals/rmargins.pdf`, page 11, option `df(#)`), vérifié le 2026-11-21 :

> df(#) specifies that the t distribution with # degrees of freedom be used for computing p-values and confidence intervals. The default typically is to use the standard normal distribution. However, if the estimation command computes the residual degrees of freedom (e(df_r)) and predict(xb) is specified with margins, the default is to use the t distribution with e(df_r) degrees of freedom.

Stata ne passe en t que pour la prédiction linéaire `xb`. spicy va plus loin pour un `glm` à dispersion estimée, dont l'AME était déjà testé en t avant ce lot. Ce point n'est pas rouvert ici.

## Ne changent pas

- Un `lm()` ou un `glm()` sous `vcov = "CR*"` : l'AME garde ses degrés de liberté de Satterthwaite (contraste exact de clubSandwich pour `lm`, coefficient dominant pour `glm`) et la phrase « AME inference: t-test with Satterthwaite df. ».
- Les plans d'enquête : degrés de liberté du plan, passés explicitement.
- Les ajustements bayésiens : pas de test.
- Les classes en z : `glm` binomial et Poisson, `rlm`, `lmer` sans lmerTest, `glmer`, glmmTMB, ordinaux, multinomiaux, pscl, survie, `geeglm`, betareg, rms hors `ols`, `gam` non gaussiens.

## Classes qui bougent

Balayage des fixtures de `helper-class-fixtures.R` (240 observations), test et `df` de l'AME :

| Fixture | Coefficients | AME avant | AME après |
|---|---|---|---|
| `lm_robust`, `lm_robust` classique | t(238) | z | t(238) |
| `lm_robust_fe` | t(233) | z | t(233) |
| `iv_robust`, `ivreg` | t(238) | z | t(238) |
| `gls`, `ols`, `ols_xy`, `rq` | t(238) | z | t(238) |
| `gam` gaussien | t(237) | z | t(237), smooth compris |
| `lme` | t(233) | z | t(233) |
| `lme_contrasts`, `lme` avec interaction | t(231) | z | t(231) |
| `lmerTest::lmer(y ~ x + z + (1 \| g))` | t(7,04 ; 232,23 ; 233,29) | z | t(232,23 ; 233,29) |
| `lm_robust(clusters = id)` CR2 | t(46,21 ; 38,29 ; 49,81) | z | t(38,29 ; 49,81) |

## La note

Règle 1 : aucune phrase, celle des coefficients vaut pour les AME. Règle 2 : une phrase, attribuée au modèle dans un tableau de plusieurs modèles.

- « AME inference: t-test on the smallest df among the coefficients each effect depends on. »
- « Inférence sur les AME : test t sur le plus petit nombre de degrés de liberté parmi les coefficients dont dépend chaque effet. »

## Gardes

`tests/testthat/test-ame_reference_distribution.R` : balayage de propriété sur les fixtures (z donne z, un `df` partagé donne ce `df`, des `df` différents donnent un `df` pris parmi eux), liste fermée des quatre exceptions, étalons sur n = 24 (`ols`, `gls`, `ivreg`, `lm_robust` et `iv_robust` classiques contre `lm` et `avg_slopes(df = 21)`), `lme` contre `emmeans::emtrends(mode = "containment")` sur un prédicteur entre groupes seul et pris dans une interaction, lmerTest, la note en anglais et en français.
