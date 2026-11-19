# ADR 2026-11-19 — Les termes d'erreur et la méthode d'un modèle de sélection

**Statut** : principe de l'affichage approuvé par Amal le 2026-11-19, décisions de détail du même jour. Mis en œuvre dans la branche `lot-selection-error-terms`. Rupture assumée (version < 1).

## Contexte

Exemple de l'aide de `sampleSelection::selection()` (Greene 2003, ex. 22.8) : `Mroz87`, 753 femmes dont 428 actives, `selection(lfp ~ age + I(age^2) + faminc + kids + educ, wage ~ exper + I(exper^2) + educ + city)`.

Le tableau montrait les deux équations, une ligne par variable avec ses sous-lignes `selection` et `outcome`, et les nombres de `summary()`. Il laissait de côté ce qui dit si la sélection compte :

- En maximum de vraisemblance, σ = 3,1084 (0,1138) et ρ = −0,1320 (0,1651), p = 0,424. spicy les calculait (`extras$selection_sigma`, `selection_rho`) sans les afficher.
- En deux étapes (`heckit()`), le coefficient du ratio de Mills inverse, −1,098 (1,266), p = 0,386, puis σ = 3,200 et ρ = −0,343 sans erreur standard. Rien n'était affiché.
- La méthode d'estimation, calculée (`extras$estimation_method`) et jamais écrite. Un tableau deux étapes et ML côte à côte portait la même note pour les deux colonnes.
- Le nombre de cas de l'équation de résultat (428). La ligne n donnait 753.

Sept défauts voisins, relevés le même jour (dossier de l'article sur les modèles additifs, de proportions et de sélection, trouvailles F4 à F17) :

- Un modèle à deux régimes (tobit-5) gardait 2 coefficients sur 10. Le code lit `param$index$betaO`, qui n'existe pas en tobit-5 (`betaO1`, `betaO2`).
- `nested = TRUE` sur deux ajustements de sélection levait l'erreur interne « 'formula' incorrecte » : `stats::formula()` n'a pas de méthode pour la classe.
- Un `treatReg()` était titré « Heckman selection model ».
- Les valeurs p étaient celles de `summary()`, un t à 740 degrés de liberté (0,005345 pour `age` en sélection), sous l'étiquette z (z donnerait 0,00521). Les intervalles venaient de `qnorm()`.
- `exponentiate = TRUE` avertissait d'un « identity-link fit » alors que l'équation de sélection est un probit.
- Un résultat binaire (équation de résultat probit) n'était signalé nulle part.

## Décision

Un bloc « Error terms » sous les coefficients, affiché par défaut. C'est le libellé de `summary()`. Il passe par le rail des blocs subordonnés, comme les « Scale effects » d'un `clm` :

- Le cadre range les lignes dans `info$extras$error_terms` (`.selection_error_rows()`, `R/regression_frame_flexsurv_selection.R`).
- `table_regression()` les ajoute après l'exponentiation et l'ajustement des p, sous `show_components`. Elles restent hors de la famille de `p_adjust`, comme le compte de la note.
- L'identité du bloc est `.REG_BLOCK_ERROR` dans `R/regression_align.R`, avec sa légende `label_block_error_terms`.

Les lignes, symbole en tête comme celles des effets aléatoires : « σ (outcome error) », « ρ (error correlation) », « λ (inverse Mills ratio) ». En français : « σ (erreur du résultat) », « ρ (corrélation des erreurs) », « λ (ratio de Mills inverse) ».

- Maximum de vraisemblance : σ et ρ avec estimation, erreur standard et intervalle. La valeur p de ρ est celle de `summary()`. Celle de σ reste vide, car personne ne teste σ = 0. Un résultat binaire n'a que ρ, sa variance d'erreur étant fixée à 1.
- Deux étapes : λ avec estimation, erreur standard, intervalle et p, puis σ et ρ avec leur seule estimation.

Chaque nombre égale `summary(fit)$estimate` à 1e-10.

`keep` et `drop` filtrent ces lignes par leur nom de coefficient dans `coef(fit)` (`sigma`, `rho`, `invMillsRatio`). Les autres blocs restent exemptés. La correspondance passe par `.reg_filter_terms()` dans `R/regression_transform.R`, que la vérification des motifs sans effet et le pilote de `table_stack()` lisent aussi. La note ne définit que les lignes restées dans le tableau.

La note gagne quatre phrases :

- « Outcome observed for 428 of 753. », pour un tobit-2 seulement. Un modèle d'effet de traitement observe le résultat pour tous les cas.
- La méthode d'estimation, à la place de l'ancien libellé de variance : « Std. errors: maximum likelihood. » ou « two-step ». Deux modèles côte à côte donnent « Model 1: two-step, Model 2: maximum likelihood ».
- « P-values and CIs: t distribution with 740 df. » : `summary()` teste chaque ligne contre un t sur `param$df` (`coefTable(est, se, df)`), en ML comme en deux étapes. Les lignes portent maintenant `test_type = "t"` et ce df, et les intervalles viennent de `qt()`.
- La définition des lignes du bloc : « Error terms: σ, standard deviation of the outcome error; ρ, correlation of the selection and outcome errors. » En deux étapes, λ est défini comme une estimation de ρσ, et une phrase dit que σ et ρ sont déduits des estimations, sans erreur standard.

Dans un tableau qui mêle un autre modèle, les phrases propres au modèle de sélection sont précédées de son nom. Sinon « t on 740 df » se lirait aussi pour un `lm` à côté.

Titres : « Heckman selection model (binary outcome) » pour un résultat binaire, que `summary()` annonce « Tobit 2 model with binary outcome ». « Treatment-effects model » pour `treatReg()`, que `summary()` annonce « Tobit treatment model ». Les lignes d'un `treatReg()` égalent `summary()` à 1e-10 (vérifié). Le titre exact suffit donc. Traductions dans le pont des titres et leurs pluriels.

Refus :

- Un tobit-5 est refusé par `.refuse_inherited_subclass()` (`spicy_unsupported_class`). Le message dit que le tableau couvre le tobit-2 et le modèle d'effet de traitement.
- `nested = TRUE` refuse un ajustement en deux étapes, qui n'a pas de vraisemblance, et le mélange d'un modèle de sélection avec une autre classe (`spicy_invalid_input`).
- `exponentiate = TRUE` garde les coefficients sur leur échelle et le dit sans parler de lien identité.

## Ce qui a été mesuré

- Lignes d'erreur, ML et deux étapes, contre `summary()` : égales à 1e-10 (estimation, erreur standard, p, intervalle recalculé par `qt()`).
- `treatReg()` : lignes des deux équations et σ, ρ égales à `summary()` à 1e-10.
- `nested = TRUE` sur deux ML emboîtés (sans puis avec `city`) : LR 1,993 sur 1 df, p = 0,158, égal au calcul à la main sur `logLik()`. La mécanique existante y suffisait. `anova()` n'a pas de méthode pour la classe, et le repli sur les log-vraisemblances (`loglik_lrt()`) donne le test. Il manquait seulement la lecture du nom du résultat dans la validation.
- Sorties : console, `output = "data.frame"`, `"long"`, `tidy()`, gt, tinytable, flextable, Excel et `as_structured()` portent les lignes du bloc.

## Ce qui a été écarté

- Un argument propre au bloc. `show_components` le gouverne déjà, comme les composantes des modèles de comptage et les effets d'échelle d'un `clm`.
- Un jeton d'ajustement pour le nombre de cas du résultat. Une phrase de note suffit et ne crée pas de ligne vide pour les autres classes.
- La prise en charge du tobit-5 : deux équations de résultat, deux σ et deux ρ, un bloc de plus et des titres à écrire. C'est une fonctionnalité à part. Le refus ne l'annonce pas.
- Des intervalles de ρ bornés à [−1, 1]. L'intervalle reste celui de Wald sur l'échelle de ρ, comme dans `summary()`, et peut sortir de ces bornes quand ρ est proche de ±1.

## Limites

- Le titre porte encore « outcome » au lieu du nom de la variable de résultat. Ce défaut est traité dans le lot des titres.
- `.mixed_inference_label_for_frame()`, qui porte la ligne des valeurs p, sert maintenant aussi au modèle de sélection. Son nom date des modèles mixtes.
