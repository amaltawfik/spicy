# ADR 2026-10-20 — Tableaux survey : intervalles à 0 % et 100 %, colonnes du plan de sondage

**Statut** : accepté (2026-10-20, Amal : « je suis d'accord, fais ce qu'il y a de plus pro et robuste, pas de demi-mesure »).

## 1. Intervalle d'un pourcentage de 0 ou 100

**Défaut.** Avec `proportion_ci = TRUE`, `table_categorical_svy()` affichait « 0.0 [0.0, 0.0] » ou « 100.0 [100.0, 100.0] » dans un groupe où une modalité est absente ou seule. Un intervalle de largeur nulle affirme une certitude. La cause est dans `survey::svyciprop()` (survey 4.5), dont l'aide le dit : « None of the methods will work when the observed proportion is exactly 0 or 1. » La méthode logit ne converge pas (estimation 2,8e-12, avertissement non classé et traduit de `glm.fit`). La méthode likelihood échoue au profilage. Les méthodes asin, beta, xlogit et wilson rendent NaN, et mean rend [0, 0].

**Décision.** À une proportion de 0 ou 1, quelle que soit `ci_method`, l'intervalle est l'intervalle binomial exact (Clopper-Pearson) sur l'effectif non pondéré du domaine, avec x = 0 ou n et sans facteur de degrés de liberté :

- p = 0 : [0, 1 − (α/2)^(1/n)] ;
- p = 1 : [(α/2)^(1/n), 1].

Sans observation dans le domaine (n = 0), il n'y a pas d'intervalle. La note le dit en une phrase. Aucun avertissement de survey ne remonte.

**Sources.**

- Korn et Graubard (1998), *Survey Methodology* 24(2), p. 194 : « Both n* and n*_df are set equal to n when p̂ = 0. » Pour la méthode logit : « when p̂ = 0, we set the confidence interval equal to the binomial interval (p_L(0,n), p_U(0,n)). » Pour un domaine, « the sample size n is taken to be equal to the sample size restricted to the subdomain ».
- Parker et al. (2017), *NCHS Data Presentation Standards for Proportions*, Vital Health Stat 2(175), p. 2 : « If the number of events is 0 (or its complement), then the denominator sample size should be used to obtain confidence intervals. » Et p. 3 : la taille d'échantillon « should also be used for CIs and other computations that include the effective sample size ».
- La macro SAS officielle du NCHS (`KG_macro.sas`, 2021) applique la même règle à 0 et à 1 avec `n_eff_df = n`.
- Le paquet R `surveytable` 0.10.0 du NCHS donne le même intervalle à 0 (n = 100 : [0, 0,03621669]) et NaN à 1.

**Non retenu.** Laisser la cellule vide : honnête, mais cela perd une borne que les sources savent calculer. Appliquer le facteur de degrés de liberté aux bornes : aucune source ne le fait.

## 2. Colonnes techniques du plan de sondage dans `select`

**Défaut.** `table_continuous_svy(des, select = everything())` résumait les poids, les identifiants d'unités primaires et les strates comme des variables. La version data frame écarte la colonne des poids depuis le lot A (2026-10-20), qui refuse une colonne de rôle nommée dans `select` et la fait sauter par les sélecteurs.

**Décision.** Les poids d'échantillonnage, les poids répliqués, les identifiants de grappe (à chaque degré) et les colonnes de correction de population finie ne sont pas des variables à décrire. Les sélecteurs les sautent. En nommer une (par son nom, une chaîne ou sa position) est une erreur de la même classe que pour une colonne de rôle, qui nomme la colonne, son rôle dans le plan, et la fonction data frame qui permet de la décrire. Les strates restent des variables ordinaires : une variable de stratification comme la région est souvent une variable d'intérêt. Une colonne du plan reste permise comme `by`. Les colonnes se lisent dans l'objet survey lui-même (identifiants, fpc, formules des poids, poids répliqués), jamais d'après leur nom.

## Conséquences

Deux puces dans NEWS : un correctif pour les intervalles aux bornes, une rupture pour les colonnes du plan (comportement publié en 0.13.0).
