# ADR 2026-11-19 — Aucun jeton muet dans `show_fit_stats`

**Statut** : approuvé par Amal le 2026-11-19. Mis en œuvre le même jour, branche `lot-requested-rows`. Rupture assumée (version < 1).

## Contexte

`table_regression()` refusait déjà beaucoup de jetons hors de leur classe, par des contrôles par famille dans `validate_class_appropriate_tokens()`. Ces contrôles avaient des trous. Le balayage du 18.11 (ADR `2026-11-18-fit-stats-tokens.md`) prend un ajustement léger pour chacune de 44 classes et les 47 jetons autres que `nobs`, soit 2 068 appels `table_regression(fit, show_fit_stats = c("nobs", jeton))`. Hors jetons de variation, il comptait 855 paires refusées, 269 rendues et 504 muettes : le jeton passait la validation, la ligne manquait et rien ne le disait. Les jetons de variation demandés sans `nested = TRUE` donnaient 146 refus et 294 paires muettes. Exemples : `icc` hors modèles mixtes, `n_events` hors Cox, `rmse` hors `lm` et `glm`, `r2` d'un `polr`, `weighted_nobs` d'un modèle sans poids.

L'ADR `2026-11-14-pseudo-r2-defaults.md` avait posé la règle pour les pseudo-R² : le tiret dans la cellule d'un modèle hors du domaine quand un autre modèle a la valeur, le refus quand aucun modèle n'est dans le domaine. Le contrôle de `log_lik` après la construction des cadres (`.reg_check_log_lik_rows()`, lot du 18.11) l'appliquait à un seul jeton.

## Décision

1. Un jeton nommé dans l'appel, qu'aucun modèle du tableau ne peut afficher, est refusé par une erreur `spicy_invalid_input` qui le nomme. L'erreur nomme `table_regression()` ou, quand l'appel vient de l'écran univarié, `table_regression_uv()`.
2. Un seul contrôle général, `.reg_check_requested_rows()` dans `R/table_regression.R`. Il tourne une fois les cadres construits et les statistiques de variation attachées, là où l'on sait quelles lignes chaque jeton produit. Il applique aux cadres le test des constructeurs de lignes (`build_fit_stats_rows()`) : `fixed_effects` produit un bloc quand un modèle absorbe un facteur, `n_groups` quand un modèle a des groupes, tout autre jeton quand un modèle au moins a une valeur. Aucune liste de classes n'intervient dans la décision.
3. `.reg_check_log_lik_rows()` est replié dans ce contrôle. Quand `log_lik` est seul en cause, son message est gardé mot pour mot.
4. Les refus par famille en amont restent, avec leurs messages, et passent en premier.
5. Un jeton de variation (`r2_change`, `f_change`, `p_change`…) demandé sans `nested = TRUE` est refusé par un message qui dit qu'il demande `nested = TRUE` et au moins deux modèles.
6. Le message donne une raison par jeton seulement là où le domaine est vérifié dans le code et seulement quand aucun modèle du tableau n'est de ce domaine : `log_lik`, `weighted_nobs` (modèle pondéré ou plan d'enquête), `n_events` (Cox), `icc`, `r2_marginal` et `r2_conditional` (modèles mixtes de lme4, glmmTMB et nlme), `n_groups`, `fixed_effects`, `eff_p` (`svyglm`). Sinon il renvoie à la section `show_fit_stats` de l'aide. Un écran univarié reçoit une phrase de plus : ses modèles univariés n'ont pas de statistiques d'ajustement.
7. Périmètre : les jetons passés dans l'appel. Laissé au défaut, `show_fit_stats` ne change pas de comportement : un jeton du défaut sans valeur reste sauté. Aucune option globale ni `spicy_style()` ne fournit ces jetons. `.STYLE_ARG_FIELDS` ne contient que des réglages de format, et aucun `getOption()` ne lit `show_fit_stats`.
8. Plusieurs modèles : si un modèle au moins affiche le jeton, pas d'erreur, tiret pour les autres. Rien ne change ici.
9. Disposition empilée (`layout = "stacked"`) : la pile est un seul tableau. Chaque panneau laisse le contrôle au pilote, qui l'applique aux cadres de tous les panneaux. Un jeton affiché par un panneau passe pour toute la pile.

Aucun défaut ne change, aucun nombre ne bouge, aucun message de refus existant n'est réécrit.

## Alternatives écartées

- **Toujours afficher la ligne demandée, avec un tiret dans chaque cellule.** Une ligne vide n'informe personne. Elle contredirait les 855 refus existants, qui suivent la règle inverse depuis la 0.13.
- **Le statu quo.** Une demande explicite sans effet et sans message est le pire des cas : l'utilisateur croit le tableau complet.
- **Compléter les contrôles par famille.** Il faudrait une liste par jeton et par classe, à tenir à jour pour chaque classe ajoutée. Le contrôle sur les cadres couvre d'office une classe ou un jeton futurs.

## Ce qui a été mesuré après le lot

Même balayage, mêmes 44 classes, sur la branche. La ligne compte désormais toutes les lignes du tableau, et non les seules lignes `fit_stat`, pour voir le bloc `fixed_effects`. Hors jetons de variation : 1 359 paires refusées, 269 rendues, aucune muette, aucune autre erreur. Jetons de variation sans `nested = TRUE` : 440 refusées, aucune muette. Les 269 paires rendues sont celles d'avant le lot.

Treize blocs de tests existants attendaient un saut silencieux et ont été convertis en refus. Six portent sur `sigma` : `glm.nb`, `glmer.nb` seul et emboîté, GAM `betar()` à côté de `betareg`, GAM `scat()`, `survreg` stratifié. Les autres portent sur `aic` et `eff_p` d'un `svyglm` quasi-binomial sans constante, `eff_p` d'un `glm`, `weighted_nobs` d'un `glm` sans poids, `within_r2` d'un `feols` instrumental, Tjur d'un écran univarié sans modèle multivarié, `r2_change` sans `nested = TRUE`. Le dernier est un tableau sans coefficients (`show_columns = "none"`) qui demande `n_events` pour un `lm` : il recevait l'erreur `spicy_empty_table` (« None of the requested fit statistics is available »), il reçoit désormais le refus qui nomme le jeton. Ce message reste celui d'un tableau sans coefficients que le défaut laisse vide.

## Limites

- Certains refus tiennent aux données et non à la classe : `sigma` d'un `survreg` stratifié (une échelle par strate), `aic` et `eff_p` d'un `svyglm` quasi-binomial sans constante, `weighted_nobs` d'un modèle sans poids, `within_r2` d'un `fepois` ou d'un `feols` instrumental. Le message est alors générique : aucun modèle n'a de valeur, et l'aide liste les domaines.
- Un jeton de variation demandé avec `nested = TRUE` pour une classe dont le test emboîté ne le calcule pas reçoit le message générique, pas celui de `nested`.
- Les messages restent en anglais, comme toutes les erreurs du package. Ils ne passent pas par le registre des chaînes.
- Le balayage porte sur des tableaux d'un seul modèle. Les tableaux à plusieurs modèles, la pile et l'écran univarié sont couverts par des tests ciblés.

## Garde

`tests/testthat/test-fit_stats_requested_rows.R` : le balayage devenu test, une classe par bloc, chaque jeton du vocabulaire lu dans `.regression_tokens` (un jeton futur est balayé d'office), refus ou ligne de plus. `skip_on_cran()`, `skip_if_not_installed()` par classe, Stan en local seulement par le cache des fixtures. S'y ajoutent les tests ciblés : message et appel nommé, plusieurs jetons dans un message, renvoi à l'aide, défaut tolérant, tiret à côté d'un autre modèle, jetons de variation sans `nested`, disposition empilée, `table_regression_uv()`.
