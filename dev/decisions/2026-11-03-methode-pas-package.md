# ADR 2026-11-03 — La méthode, jamais le package

**Statut** : décidé par Amal le 2026-11-03 pour les notes, étendu aux titres le même jour. Remplace la clause d'application de la décision 42 (`dev/decisions_amal_2026-08.md`) qui traitait les suffixes moteur des titres comme des noms propres : « (glmmTMB) », « (nlme) », « (stanreg) » et « (brmsfit) » étaient détachés avant la recherche dans le pont des préfixes puis rattachés tels quels.

## Règle

Le texte d'un tableau, sa note comme son titre, nomme la méthode statistique. Il ne nomme jamais le package ni le moteur qui l'a calculée.

## Raisons

1. **Reproductibilité.** Le lecteur d'une publication doit pouvoir refaire la méthode avec n'importe quel logiciel. « Satterthwaite t-test » se refait partout, « (lmerTest) » ne dit pas ce qui a été fait.
2. **Le logiciel a sa place ailleurs**, dans la section des méthodes et dans sa citation.
3. **Un nom de package vieillit mal.** Figé dans un tableau publié, il devient faux le jour où le calcul passe par un autre moteur.

## Ce qui a changé

- Valeurs p : « P-values: Satterthwaite t-test. » au lieu de « … (lmerTest). ».
- Survey : les intervalles des pourcentages et les quantiles portent le nom de la méthode (« Wilson score », « Hyndman-Fan type 1 ») au lieu de `survey::svyciprop` ou de `qrule = "math"`.
- mgcv : « Bayesian (posterior covariance) » au lieu de « Bayesian (mgcv posterior covariance) ».
- Les remèdes aux valeurs PSIS-LOO et WAIC peu fiables (`loo::loo_moment_match()`, réajustement des plis signalés, validation croisée K-fold) quittent la note pour l'avertissement : ils s'adressent à l'analyste.
- L'invitation à réajuster un `lme4::lmer()` avec lmerTest devient un message, affiché une fois par session.
- Titres : « Linear mixed-effects regression » au lieu de « … (glmmTMB) » ou « … (nlme) », de même pour rms (« (rms) », « (rms Glm) »), mlogit, rstanarm (« (stanreg) ») et brms (« (brmsfit) »). Les qualificatifs qui nomment le modèle restent : « (proportional odds) », « (zero-inflated) », « (GEE) », « (fixed effects) ». Un tableau qui compare un `lme4::lmer()` et un `glmmTMB()` du même modèle porte un seul titre, « Linear mixed-effects regression comparison: Reaction ». Le pont français est lu sans suffixe à détacher. Il gagne les titres qui lui manquaient : les autres familles mixtes de glmmTMB et de lme4, l'ordinal mixte, les moindres carrés généralisés et le logit multinomial de choix discret.
- glmmTMB : le nom de la famille vient d'un seul libellé, partagé par le titre et par le modèle nul du test des effets aléatoires. `nbinom1` se lit « Negative-binomial (NB1) », `nbinom2` reste « Negative-binomial » (la forme standard), et la ligne du test ne montre plus le code de famille de glmmTMB (« nbinom2 regression » devient « negative-binomial regression »).

## Garde

`tests/testthat/test-i18n_language.R`, section 2, « a table note names the method, never a package » : aucune clé `note_*` d'aucune langue ne contient `(pkg)`, `pkg::` ou `` `pkg` `` pour un package d'Imports ou de Suggests, et aucun titre du pont non plus (ni `(stanreg)` ou `(brmsfit)`). Les tests de chaque classe (glmmTMB, nlme, rms et mlogit, puis rstanarm et brms dans les tests Stan locaux) vérifient que le titre ne porte aucun moteur entre parenthèses.
