# ADR 2026-09-18 — Avec `nested = TRUE`, le test de changement ignore `vcov`

**Statut** : arbitré par Amal le 2026-09-18. Rien dans la 0.13.0. En 0.14,
l'option 1 directement, sans la note intermédiaire. Voir « Arbitrage » en fin
de fiche.

## Contexte

`table_regression(models = list(m1, m2), nested = TRUE)` insère les
statistiques de changement entre modèles emboîtés (`r2_change`, `f_change`,
`p_change`). Pour les `lm`, elles viennent de `compute_one_pair_lm()`
(`R/regression_nested.R`, ligne 493), qui appelle
`stats::anova(fit_prev, fit_curr)` (ligne 508). C'est le F partiel classique,
sous hypothèse de variance constante.

L'argument `vcov` n'entre pas dans ce calcul. Quand l'utilisateur demande
`vcov = "HC3"`, les coefficients, leurs intervalles et leurs p sont robustes,
mais le test du bloc ne l'est pas. Rien dans le tableau ni dans la note de
pied ne le signale. Le tableau mélange donc deux méthodes d'inférence, en
silence.

Cas réel (projet sTayS, 2026-09-18), régression hiérarchique sur 219
observations, bloc de 19 échelles ajouté à 21 indicatrices :

| Test du bloc | Statistique | p |
|---|---|---|
| F classique, affiché par spicy | F(19, 178) = 6,91 | < 0,001 |
| Wald robuste HC3, calculé à la main | F(19, 178) = 9,06 | < 0,001 |

La conclusion est la même ici, mais l'écart n'est pas négligeable, et il peut
changer une conclusion dans un cas limite. Il a fallu calculer le test de
Wald à la main et nommer les deux tests dans la note du tableau.

Ce n'est pas un bug au sens strict : le F partiel classique est la
statistique attendue par défaut. C'est une incohérence dès que `vcov` n'est
pas classique, parce qu'un utilisateur qui demande HC3 s'attend à ce que tout
le tableau le respecte.

## Options

1. **Test de Wald robuste quand `vcov` n'est pas classique.** Remplacer le F
   de `anova()` par `b' V^-1 b / q` sur les coefficients ajoutés, avec la
   matrice demandée. C'est ce que font `lmtest::waldtest(m1, m2, vcov = )` et
   `car::linearHypothesis()`. Le tableau redevient cohérent. À prévoir : le
   libellé de la ligne doit distinguer « F change » et « Wald F », et le cas
   des erreurs-types en grappes doit donner les bons degrés de liberté.
2. **Garder le F classique, mais le dire.** Ajouter à la note de pied une
   phrase du registre i18n quand `nested = TRUE` et que `vcov` n'est pas
   classique : le test de changement est le F partiel classique et ne tient
   pas compte de la matrice de covariance robuste. Aucun changement de
   chiffre, plus de silence.
3. **Les deux.** Option 2 tout de suite, comme correctif peu risqué, puis
   option 1 dans une version ultérieure.

## Points à vérifier

- Un test de Wald robuste à grand nombre de contraintes tend à être trop
  généreux en petit échantillon. Dans le cas sTayS, 19 contraintes pour 219
  observations donnent 9,06 contre 6,91. La documentation devrait le dire.
- Le R² de changement ne dépend pas de `vcov`. Seuls le test et son p sont
  concernés.
- Les autres moteurs (`glm`, modèles mixtes) passent par un test du rapport
  de vraisemblance. La question d'une version robuste s'y pose autrement et
  reste hors périmètre de cette fiche.

## Recommandation provisoire

Option 3. La note de pied est un correctif immédiat qui supprime
l'incohérence silencieuse sans rien changer aux résultats existants. Le test
de Wald robuste est la bonne cible, mais il demande un arbitrage sur les
libellés et sur les degrés de liberté en présence de grappes.

## Arbitrage (Amal, 2026-09-18)

La recommandation provisoire n'est pas retenue. Rien ne change dans la 0.13.0
et la 0.14 fera directement l'option 1.

Raisons :

- La ligne s'appelle « F change ». C'est le nom consacré du F partiel
  classique de la régression hiérarchique, celui de SPSS et des manuels. Le
  libellé nomme donc le test. Le pied de table dit « Std. errors: HC3 », ce qui
  décrit les erreurs-types des coefficients et ne prétend rien sur le test de
  bloc. Aucun chiffre n'est faux et aucune étiquette ne ment.
- Le précédent `note_nested_ml_refit` ne s'applique pas. Il couvre des critères
  REML affichés à côté de changements calculés en ML, ce qu'aucun libellé ne
  permet de deviner. Ici le libellé porte l'information.
- La note de l'option 2 serait provisoire. Elle serait réécrite ou supprimée
  dès que le test de Wald robuste arrive, soit deux changements visibles du
  pied de table pour un seul sujet.
- La 0.13.0 est gelée pour une soumission le 30 septembre. Le gel a déjà été
  rouvert plusieurs fois, à chaque fois pour un vrai défaut. Une phrase de
  confort ne passe pas ce seuil.

Réserve consignée : le libellé renseigne un lecteur averti, pas tous. Un
utilisateur qui demande HC3 peut croire que tout le tableau suit, et c'est ce
qui s'est produit dans sTayS avant le calcul à la main. Le sujet est réel, et
la bonne réponse est le test lui-même.

Cible 0.14 : test de Wald robuste quand `vcov` n'est pas classique, avec un
libellé distinct de « F change », les bons degrés de liberté en présence de
grappes, une validation contre `lmtest::waldtest()` et
`car::linearHypothesis()`, et la mise en garde documentée sur les tests à
grand nombre de contraintes en petit échantillon.
