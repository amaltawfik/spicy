# ADR 2026-11-18 — Trois jetons de `show_fit_stats` : `log_lik`, `concordance` et le R² de déviance des GAM

**Statut** : plan approuvé par Amal le 2026-11-18 (version A : jetons sur demande, aucun défaut changé). Mis en œuvre le même jour, branche `lot-fit-tokens`.

## Contexte

`dev/fit_stats_by_class.md` relevait trois statistiques présentes dans les cadres sans jeton pour les afficher. La log-vraisemblance était stockée (`fit_stats$log_lik`) et ne servait qu'au calcul de l'AICc. La concordance d'un Cox n'existait que dans une phrase de la note. La déviance expliquée de mgcv était stockée (`pseudo_r2$dev_explained`) sans ligne.

Le critère est celui de l'ADR `2026-11-14-pseudo-r2-defaults.md`. Un nombre ne s'affiche jamais sous deux noms. Une cellule porte le tiret quand la mesure n'est pas définie pour son modèle. Un jeton qu'aucun modèle du tableau ne définit est refusé (`spicy_invalid_input`). Un défaut ne change pas sans appui des logiciels de référence et de la littérature.

## Décisions

1. **`log_lik`**, libellé « Log-likelihood » / « Log-vraisemblance », décimales `ic_digits`. La valeur est `stats::logLik()` du modèle, lue dans le cadre : la vraisemblance dont l'AIC de la même colonne est tiré. C'est la vraisemblance restreinte d'un modèle REML. Pour un `glm()` binomial, c'est la valeur de `logLik()` quelle que soit la forme des données : elle inclut les coefficients binomiaux d'une réponse groupée et arrondit les poids non entiers, à la différence des lignes de pseudo-R². L'aide le dit. Le jeton rejoint les refus de l'AIC pour les tableaux tout bayésiens et tout GEE. Il est refusé, après la construction des cadres, quand aucun modèle n'a de log-vraisemblance (famille quasi, plan d'enquête, 2SLS). Il rejoint `.REG_IC_TOKENS`, donc l'avertissement des critères comparés sur des échantillons différents. Le cadre stan pose `log_lik = NA_real_` : `logLik()` d'un brmsfit est la matrice tirages × observations, et la compaction en aurait affiché le premier élément.
2. **`concordance`**, libellé « Concordance » dans les deux langues, décimales `fit_digits`. Domaine : `survival::coxph` et `survey::svycoxph` (`.carries_concordance()`). La valeur est le C de `summary()` (coxph) ou de `fit$concordance` (svycoxph), déjà lu pour la note. Quand la ligne est affichée, la phrase de concordance quitte la note. Sinon la note reste telle quelle, avec l'erreur standard. `rms::cph` reçoit le tiret et n'a pas de phrase (voir la sonde 1).
3. **`r2_deviance` ouvert aux GAM**, sans nouveau jeton et sous le même libellé, « R² (deviance) ». La déviance expliquée de mgcv est le même nombre que le R² de déviance du glm jumeau (sonde 2). Un jeton `dev_explained` l'aurait affiché sous un second nom à côté d'un glm. Domaine : celui du glm jumeau, familles de comptage, continues et quasi (`.glm_carries_r2_deviance()`), donc aussi `nb()` et `tw()`. Hors domaine : binomial, quasi-binomial (mgcv calcule par ligne une réponse groupée, spicy par essai), `betar`, `ocat`, `scat`, `ziP`, `cox.ph`. Le refus d'un GAM binomial ne renvoie pas à McFadden, que le cadre GAM ne calcule pas.
4. **Défauts vus en passant**, corrigés dans le lot après un test qui échouait avant :
   - le R² d'un GAM à côté d'un `lm()` affichait `r.sq` de mgcv, qui est ajusté, le nombre de la ligne R² ajusté. Le cadre pose `r_squared = NA` et garde `adj_r_squared = r.sq` ;
   - la phrase de concordance sans erreur standard était écrite en dur. Elle passe par la clé `note_survival_concordance`, avec sa chaîne française ;
   - `pseudo_r2_mcfadden`, `pseudo_r2_nagelkerke` et `pseudo_r2_tjur` sur un tableau tout GAM, `deviance`, `bic` et `aicc` sur un tableau tout plan d'enquête (`aic` aussi quand aucun modèle n'est un `svyglm`) passaient la validation et ne rendaient rien. Ils sont refusés. À côté d'autres modèles, la cellule porte le tiret.

Les défauts du GAM et du Cox ne changent pas.

## Ce qui a été mesuré

**Sonde 1, `rms::cph`.** 300 observations, temps et prédicteur avec ex æquo (temps arrondis à 0,5, x à 0,1), C de `summary(coxph)` du jumeau comme référence :

| Cas | coxph | `concordance(cph)` | (Dxy + 1)/2 |
|:--|--:|--:|--:|
| simple | 0,5427536232 | 0,5427536232 | 0,5427536232 |
| strates | 0,5421217347 | 0,5415942029 | 0,5415942029 |
| poids | 0,5527018416 | 0,5527018416 | 0,5445249597 |

`survival::concordance()` marche sur un `cph` avec et sans `x = TRUE, y = TRUE`, mais sous des strates il rend un troisième nombre, ni le C stratifié du jumeau ni le C non stratifié de son prédicteur linéaire (0,5399355878). (Dxy + 1)/2 échoue sous strates et sous poids. Aucune des deux voies n'est exacte : `cph` est laissé hors domaine.

**Sonde 2, GAM purement paramétrique contre glm jumeau** (`y ~ x + z`, 400 observations), écart entre `summary(gam)$dev.expl` et le R² de déviance du glm (`compute_r2_deviance()`, déviance nulle réajustée avec l'offset et les poids) :

- gaussienne, Poisson, Gamma, quasi-Poisson, `quasi(log, mu)`, avec et sans offset, avec et sans poids : au plus 4,4e-13 ;
- gaussienne log et inverse gaussienne log, mêmes quatre cas : au plus 2,0e-12 (avec la méthode par défaut de mgcv : sous `method = "ML"`, un GAM sans terme lisse de ces deux familles échoue sur une erreur LAPACK) ;
- `nb()` contre `glm(negative.binomial(θ))` au θ du GAM : 1,5e-11 sans offset, 2,3e-11 avec. Contre `glm.nb()`, dont le θ diffère de 1e-6 : 8,9e-9 et 7,0e-12 ;
- `tw()` contre `statmod::tweedie()` à la puissance du GAM : 5,1e-10 et 5,0e-10.

La déviance nulle de mgcv traite donc l'offset et les poids comme le réajustement de spicy. `nb()` et `tw()` prennent D et D₀ au paramètre du modèle complet, la règle de l'ADR du 14.11. mgcv nomme `"Negative Binomial(θ)"` et `"Tweedie(p=…)"` la famille d'un modèle ajusté, que `.glm_r2_kind()` classe déjà en comptage et en continue.

**Reproduction des défauts** sur fe1bd2ba : `table_regression(list(lm, gam), show_fit_stats = c("nobs", "r2", "adj_r2"))` donnait 0,46207 dans les deux lignes du GAM. Les cinq jetons muets passaient sans erreur ni ligne. La phrase sans erreur standard ignorait `spicy.labels`.

## Alternatives écartées

- Un jeton `dev_explained` : deux noms pour le nombre du R² de déviance.
- Recalculer D₀ à la manière de spicy pour les GAM : inutile, la sonde 2 donne le même nombre.
- Le C de `cph` par `concordance()` ou par Dxy : inexact sous strates (sonde 1).
- La ligne de concordance et la phrase de la note ensemble : le même nombre deux fois.
- Le nom `logLik` : la casse des jetons est minuscule depuis la 0.13. `c_index` : le nom `concordance` reste ouvert au C d'un `lrm`.
- Une liste de classes pour refuser `log_lik` : le contrôle après les cadres lit la valeur elle-même et couvre toute classe sans log-vraisemblance.

## Limites

- L'AIC, le BIC et la log-vraisemblance d'un binomial à poids non entiers passent par l'arrondi des poids de `logLik()`. Hors lot.
- `adj_r2` est accepté pour un tableau tout GAM (le `r.sq` que `summary()` de mgcv imprime pour tout GAM), `r2` y est refusé avec un message propre aux GAM. Le défaut des GAM ne change pas.
- Pas de McFadden pour un GAM binomial. Sur réponse 0/1, `dev.expl` en serait exactement la valeur. Hors lot.
- `glance()` n'a pas de colonne `logLik`. Hors lot.

## Conséquences

- Taille : R/ +273/−42 sur 15 fichiers, dont une bonne part d'aide et de commentaires. Un fichier de tests neuf (592 lignes), quatre fichiers de tests retouchés, le registre des chaînes sous snapshot (+3 clés).
- NEWS : les jetons muets devenus des refus en ruptures, les trois jetons en nouveautés, le R² du GAM et la phrase de la note en corrections.

## Garde

`tests/testthat/test-fit_stats_tokens.R` : `log_lik` contre `stats::logLik()` (lm, logit, binomial groupé, poids non entiers, Poisson avec offset, Gamma, coxph, survreg, polr, lmer REML et ML, gam), contre le `lm()` à indicatrices d'un `feols`, cohérence avec l'AIC, libellés, décimales, `inline()`, tirets à côté d'un quasi et d'un GEE, refus (quasi, svyglm, GEE, bayésien), avertissement sur des échantillons différents. `concordance` contre `survival::concordance()` et `summary()` (simple, strates, poids), svycoxph, retrait de la phrase, clé de la phrase sans erreur standard, tiret à côté d'un survreg, refus sans Cox, `cph`. `r2_deviance` contre `summary(gam)$dev.expl` (gaussienne, Poisson, Gamma, quasi-Poisson, `nb()`, `tw()`, `bam`), contre le glm jumeau avec et sans offset et poids, une seule ligne à côté du glm, GAM binomial, quasi-binomial et `ocat`. Les défauts reproduits, `table_regression_uv()`. `test-regression_frame_stan.R` (local) : `log_lik` NA et refusé pour brms et rstanarm.
