# ADR 2026-11-18 — `partial_type` : les tests par terme en type II ou en type III

**Statut** : design approuvé par Amal le 2026-11-18 (« go sur les 2 design »), nom de l'argument compris. Mis en œuvre le même jour.

## Contexte

Les tests par terme de `table_regression()` (`partial_f`, `partial_chi2`) et les tailles d'effet partielles sont de type II depuis leur création (ADR `2026-11-12-partial-f.md`). Le 12.11, la relecture notait que SAS PROC GLM, SPSS UNIANOVA, `anova` de Stata et `gtsummary::add_global_p()` donnent le type III par défaut. Elle concluait : « un type III en option n'est pas prévu sans demande ».

Sans interaction dans le modèle, les deux types coïncident. Avec une interaction, ils diffèrent, et un utilisateur ne pouvait pas retrouver dans spicy le F, le χ² ni le η² partiel que rapportent SPSS, SAS ou jamovi. Pour un package qui vise toutes les traditions, c'était un trou.

## Décisions

1. **Le type II reste le défaut.** Il respecte la marginalité et ne dépend pas du codage des contrastes. Fox et Weisberg (2019, *An R Companion to Applied Regression*, 3e éd., p. 265, page 369 du PDF de la bibliothèque) : « It is difficult to go wrong if we construct tests that conform to the principle of marginality, always ignoring higher-order relatives ». Les nombres du type II ne changent pas, au dernier chiffre.
2. **Un argument, `partial_type = c("II", "III")`**, qui règle toutes les colonnes `partial_*`, comme `ame_type` règle les colonnes `ame`. Le nom évite `anova_type`, qui se lirait comme touchant aussi les lignes de changement de modèle. Les valeurs `2` et `3` sont acceptées, comme dans car, afex et lmerTest.
3. **Le type III est calculé sous contrastes à somme nulle, quel que soit le codage de l'ajustement.** Fox et Weisberg (p. 265) : « we should not use the default contr.treatment with Type III tests ». Sous `contr.treatment`, la table de type III « is different (and incorrect) » (p. 268). Le test d'un facteur A porte alors sur l'égalité de ses moyennes marginales non pondérées (p. 266). Les facteurs ordonnés gardent `contr.poly`. Pour un `lm` et un modèle mixte, une reparamétrisation suffit (`M = qr.solve(X_sum, X_fit)`, puis le Wald par bloc), sans réajustement. Pour un `glm`, le réajustement se fait sur le plan recodé.
4. **Périmètre** : toutes les classes qui ont un test par terme (lm, glm à dispersion estimée ou fixée, glm.nb, merMod, glmmTMB, lme). Les tailles d'effet partielles du `lm` suivent le type, comme dans SPSS et afex.
5. **Trois garde-fous.** Un coefficient aliasé (cellule vide) est une erreur sous le type III, comme dans car. Un facteur en interaction avec une covariable numérique donne un avertissement par tableau : son test de type III est pris là où la covariable vaut 0. Pas de centrage automatique, pour rester égal à SPSS et à SAS. Le troisième date du lot de couverture du même jour. Un facteur codé par un contraste d'utilisateur de moins de k − 1 colonnes est une erreur sous le type III (`spicy_invalid_input`, pour lm, glm, glm.nb, merMod et glmmTMB). Un tel modèle ne contient pas le facteur entier, et l'égalité de toutes ses moyennes marginales n'y est pas testable. Avant, les cellules restaient vides sous une note qui disait « (Type III) ». Un `lme` n'est pas refusé : nlme complète un contraste trop court avant d'ajuster. Le type II n'est pas touché et égale `car::Anova(type = 2)` sous un contraste réduit.
6. **La note nomme le type dans les deux cas** : « F = partial F statistic (Type II) ». `tidy()` et la sortie longue portent une colonne `partial_type`.

## Ce qui a été mesuré

Plan déséquilibré à deux facteurs avec interaction et une covariable (90 observations) :

| Terme | Type II | Type III (somme nulle) | `car::Anova(type = 3)` sur l'ajustement en contrastes de traitement |
|:--|--:|--:|--:|
| A | F = 7,19 | F = 5,72 | F = 9,43 |
| B | F = 17,46 | F = 20,26 | F = 4,23 |

- `gtsummary::add_global_p()` rend la troisième colonne par défaut, vérifié. `emmeans::joint_tests()` et afex rendent la deuxième.
- Écart maximal à `car::Anova(type = 3)` sur le réajustement recodé : 1e-13 ou mieux pour lm, lm pondéré, glm Gamma, binomial, Poisson avec offset, quasi-Poisson, quasi-binomial, Tweedie, glm.nb, lmer, glmer, lme et glmmTMB. Pour glmmTMB, l'écart est pris sur le même ajustement : entre deux ajustements distincts il atteint 1,7e-6, la tolérance de l'optimiseur.
- PSPP 2.0, `GLM /METHOD=SSTYPE(3)`, sans covariable : F = 6,17, 21,81 et 5,13, égaux à deux décimales. PSPP refuse les covariables et `ETASQ` dans cette commande.
- Sous une variance robuste (CR2), le type III d'un lmer égale `car::Anova(type = 3, vcov. = )`. Celui d'un lme égale la matrice portée par une reparamétrisation calculée à part.
- Invariance au codage : les ajustements en contrastes de traitement, à somme nulle et de Helmert donnent les mêmes cellules à 1e-10.
- Lot de couverture du même jour : sous `na.action = na.exclude`, le type III d'un `lme` laissait ses cellules vides (`fitted()` rend alors un vecteur de la longueur des données). C'est corrigé. Les statistiques égalent celles du même modèle sous `na.omit`.

## Alternatives écartées

- Laisser le type II seul, avec une phrase d'aide. C'était ma première réponse du 18.11, abandonnée après relecture de l'ADR du 12.11.
- Passer l'ajustement de l'utilisateur à `car::Anova(type = 3)`. Le résultat dépend de son codage, c'est le piège que le lot ferme.
- Centrer les covariables d'office, comme `emmeans::joint_tests()`. Le nombre ne serait plus celui de SPSS ni de SAS.
- `anova_type` ou `ss_type` pour le nom. Le premier est ambigu avec les lignes de changement, le second est faux pour les χ² de vraisemblance et de Wald.

## Limites

- Les numéros de page de Fox et Weisberg sont déduits d'un décalage de 104 pages entre le PDF et l'imprimé, lu sur l'index. À confirmer sur l'exemplaire.
- Venables et Ripley (2002) est dans la bibliothèque mais son PDF n'a pas pu être lu ce jour-là. Langsrud (2003), Venables (2000) et Nelder (1977) n'y sont pas : rien n'en est cité.
- Que le η² partiel de SPSS porte sur les sommes de carrés du type choisi relève de la connaissance générale. PSPP ne permet pas de le vérifier.
- Les termes polynomiaux (`poly()`, `I(x^2)`) sont testés en type III sans garde, comme dans car.
- clubSandwich construit le plan d'un lme sans ses contrastes : voir le lot du même jour sur ce point.

## Conséquences

- Taille : R/ +550/−55 environ, dont beaucoup de commentaires et de messages. Tests : +700 environ.
- La note publiée des statistiques partielles gagne « (Type II) ». Snapshots et tests de note mis à jour.
- Une puce de NEWS dans les nouveautés.

## Garde

`tests/testthat/test-partial_type.R` : les oracles ci-dessus classe par classe, l'invariance au codage, les valeurs de PSPP épinglées, les trois garde-fous, `na.exclude` pour un `lme`, les formes `2` et `3`, les sorties riches en anglais et en français, le criblage univarié, la variance robuste des modèles mixtes.
