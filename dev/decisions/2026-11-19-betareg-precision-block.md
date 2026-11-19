# ADR 2026-11-19 — Le sous-modèle de précision d'un `betareg` s'affiche en bloc

Statut : principe approuvé par Amal le 2026-11-19. Mis en œuvre le même jour, branche `lot-betareg-precision-block`.

## Contexte

Le dossier de l'article sur les GAM, les régressions bêta et les modèles de sélection (`dev/_source/2026-11-19/articles/gam/dossier.md`) a relevé quatre défauts de `table_regression()` sur `betareg::betareg()`.

- F2. `betareg(I(food / income) ~ income + persons | persons, data = FoodExpenditure)` a un sous-modèle de précision (lien log) : constante 5,504 (ES 0,533), `persons` −0,484 (ES 0,133), p = 0,00029. Le tableau n'en montrait rien. Il avait les trois lignes du modèle à précision constante et aucune note. Le registre disait `blocks = "-"`.
- F10. Le lien de la moyenne n'était dit nulle part. Logit, probit et loglog étaient tous titrés « Beta regression ».
- F16. Un `betareg` pondéré n'était pas reconnu : `has_weights = FALSE` était écrit en dur.
- F11. Avec des contrastes fixés dans les données (`betareg::GasolineYield`, `batch` codé `contr.treatment(base = 10)`), le facteur n'avait pas de ligne de référence.

Le package a déjà un rail pour les composantes secondaires (`dev/component_blocks_spec.md`) : excès de zéros de `pscl`, `ziformula` et `dispformula` de `glmmTMB`. Le bloc de précision y passe, sans mécanisme parallèle.

## La décision

1. Quand la précision a des covariables (`y ~ x | z`), un bloc `Precision` suit les coefficients de la moyenne. Une ligne par coefficient du sous-modèle, avec estimation, ES, IC de Wald et p du test z, lus sur `summary(fit)$coefficients$precision`. Les facteurs ont leur ligne de référence dans le bloc, indépendamment du côté de la moyenne.
2. Le bloc est construit par `.betareg_precision_block()` (`R/regression_frame_mlogit_betareg.R`) et rangé dans `info$extras$component_blocks`, comme les blocs de `pscl` et de `glmmTMB`. L'orchestrateur le promeut en lignes par `.append_component_rows()`. L'argument `show_components` le pilote, et `FALSE` l'omet avec sa note.
3. La note donne l'échelle du lien de précision : « Precision component: log scale. », « … identity scale. », « … square-root scale. ». En français : « Composante « Précision » : échelle logarithmique. », « … échelle identité. », « … échelle de la racine carrée. ».
4. `exponentiate = TRUE` suit la règle du bloc Dispersion de `glmmTMB` : jamais d'exponentiation (`exp_ok = FALSE`). La note ajoute alors « Left on the link scale (not exponentiated). ».
5. Sous `vcov = "CR1"`, les ES du bloc sont lues sur les lignes `(phi)_` de la même matrice `sandwich::vcovCL()` que celles de la moyenne. Le tableau n'a qu'un estimateur.
6. Une précision constante (`y ~ x` ou `y ~ x | 1`) ne donne aucun bloc. Le tableau est celui d'avant et le jeton `phi` est inchangé.
7. Le paramètre de dispersion de `glmmTMB::beta_family()` est la précision φ. Un `dispformula` sous cette famille prend donc le bloc `Precision` et la même note, au lieu de `Dispersion`. Les deux moteurs d'un même modèle remplissent les mêmes lignes. C'est un changement d'un affichage publié.
8. F10. Un lien de la moyenne autre que logit se lit dans le titre, sur le modèle de « Probit regression » et « Probit GAM » : « Probit beta regression », « Complementary log-log beta regression », « Log-log beta regression », « Cauchit beta regression », « Log-link beta regression ». Le logit garde « Beta regression ». « Log-link » évite qu'un lecteur prenne « Log beta regression » pour une réponse transformée. Les titres français sont dans le pont des titres.
9. F16. Un `betareg` pondéré est reconnu comme un `glm` pondéré : `weights_kind = "case"`, `has_weights`, `weighted_n` et la statistique `weighted_nobs`. Un `glm` pondéré n'a pas de note par défaut. Ce qu'il a, et qu'a maintenant le `betareg`, c'est le jeton `weighted_nobs` et la colonne de `glance()`.

## Ce qui a été mesuré

Chaque nombre du bloc égale `summary(fit)$coefficients$precision` à 1e-10 (estimation, ES, p, bornes de l'IC) : liens de précision log, identity et sqrt, un facteur à trois niveaux dans la précision, une variable présente dans les deux sous-modèles. Sous CR1, les ES égalent `sandwich::vcovCL(fit, cluster = ~persons)` à 1e-10.

`glmmTMB(share ~ income + persons, dispformula = ~persons, family = beta_family())` donne 5,50 et −0,48, comme `betareg`, avec des ES plus grandes (0,66 et 0,17 contre 0,53 et 0,13).

Avec `p_adjust = "holm"`, la pente de précision entre dans la famille (p de 0,00029 à 0,00087, trois hypothèses) et la constante n'y entre pas, comme pour les autres composantes. `keep` et `drop` agissent sur les lignes de la moyenne et laissent le bloc. `labels` ne renomme pas les lignes du bloc, comme pour les autres composantes. L'AME reste celle du sous-modèle de la moyenne. `nested = TRUE` entre un modèle à précision constante et le modèle `| persons` affiche Δχ² = 7,70, l'étalon de `lmtest::lrtest()`. Dans un tableau à deux modèles, le modèle à précision constante a des cellules vides dans le bloc.

Le bloc est présent dans la console, `output = "data.frame"`, `"long"`, `tidy()`, gt, tinytable, flextable, Excel et `as_structured()`.

Poids : pour un `betareg` et un `glm` gaussien de même structure pondérés par les mêmes poids, `weights_kind`, `weighted_n` et la ligne `Weighted n` sont identiques et égalent la somme des poids.

## Ce qui a été écarté

- F11 n'est pas corrigé. La cause n'est pas propre à `betareg` : `detect_factor_terms()` (`R/regression_extract.R`) prend le premier niveau pour référence et ne signale une référence que si le coefficient de ce niveau est absent. Avec `contr.treatment(base = 10)` ou `contr.SAS`, `batch1` existe, et aucune référence n'est trouvée. Le correctif côté B tient en quelques lignes : le seul niveau sans coefficient est la référence. Il rendrait pourtant le tableau incohérent. `marginaleffects` compare l'AME au premier niveau, quel que soit le contraste. Avec `lm()` comme avec `betareg()`, la colonne AME de `GasolineYield` donne une valeur au niveau 10 et aucune au niveau 1, tandis que la colonne B compare au niveau 10. Marquer le niveau 10 comme référence mettrait un tiret sous B et un nombre sous AME sur la même ligne. Il faudrait que l'AME compare au même niveau que le coefficient, par exemple `avg_comparisons(variables = list(batch = "pairwise"))` filtré sur les contrastes au niveau de référence, ou un `newdata` dont le facteur est réordonné. Ce travail touche le calcul de l'AME de toutes les classes et relève d'une décision à part.
- F15, le pseudo-R² de Ferrari et Cribari-Neto. Il est déjà dans le frame (`fit_stats$pseudo_r2$pseudo`) et rien ne le lit. Un jeton coûterait environ 25 lignes dans six fichiers : la liste des jetons et l'aide de `show_fit_stats`, le libellé et les décimales du rendu, la cellule du registre, la colonne de `glance()`, la garde des statistiques demandées, un test.
- Un mécanisme propre à la précision, ou un nouvel argument. Le rail des composantes fait déjà tout ce qu'il faut.
- Réutiliser le bloc `Dispersion` pour `betareg`. La précision est l'inverse d'une dispersion, et un coefficient positif y signifie moins de variance. Sous le libellé « Dispersion », le signe se lirait à l'envers.
