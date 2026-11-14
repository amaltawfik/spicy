# ADR 2026-11-14 — Les R² par défaut des glm et des modèles catégoriels

**Statut** : décidé par Amal le 2026-11-14, sur les lectures et les
mesures ci-dessous. Commits de la branche `lot-pseudo-r2` : défauts
(f3dc4128), essais individuels (27571f0f), R² de déviance (77d11af5).

## Contexte

Depuis le lancement du `glm` dans `table_regression()` (commit 09f5e171,
2026-05-09), un `glm` affichait par défaut deux pseudo-R², McFadden et
Nagelkerke. `polr`, `clm`, `multinom` et `rms::lrm` ont ensuite reçu le
même couple. Ce couple n'a fait l'objet d'aucune décision. La note de conception d'origine
(`dev/table_regression_design.md`, ligne 601) prévoyait Tjur et McFadden
pour le logit binaire, McFadden et Cox-Snell pour le probit, McFadden
et un R² de déviance pour Poisson, « Default per family decided in
Phase 3 ». La phase 3 a livré le même couple pour toutes les familles.

L'examen du 2026-11-13 a relevé trois défauts du comportement livré.

1. Nagelkerke par défaut, alors que les sources qui comparent les
   mesures le déconseillent (section Sources relues).
2. Les valeurs dépendaient de la mise en forme des données binomiales :
   les mêmes 300 essais donnaient d'autres R² en 30 lignes `cbind()` de
   10 essais qu'en 300 lignes 0/1 (Mesures, point 3).
3. Les familles continues (Gamma, inverse gaussienne, gaussienne)
   recevaient McFadden et Nagelkerke, qui y dépendent de l'unité de y
   (Mesures, point 4).

## Sources relues

Passages relus le 2026-11-14 dans les PDF du magasin (extraction
pdftools), pages de la revue.

- Tjur (2009), *The American Statistician* 63(4).
  p. 366 (résumé) : « We propose the name “the coefficient of
  discrimination” for this statistic, and recommend its use as a
  standard measure of explanatory power. »
  p. 367 : « we consider only models for proper binary data, without
  aggregation to binomial counts. » Et sur l'égalité entre la moyenne
  des probabilités ajustées et la proportion observée : « This is an
  exclusive property of the logit models, [...] but not by models for
  binary data specified by other link functions than logit. »
  p. 369 : D = π̂̄₁ − π̂̄₀, Corollaire 1 : « D ≥ 0, with equality if and
  only if all π̂i are equal. D ≤ 1 ». La preuve (p. 371) compare la
  vraisemblance au modèle de la seule constante, ce qui suppose un logit
  avec constante et sans offset.
  p. 370 : « It is impossible to hide any longer that I consider the
  quantity D, the coefficient of discrimination, a highly recommendable
  R²-substitute for logistic regression models. » Puis : « Neither D, nor
  any of the three R² versions discussed in Section 2, have the property
  that they will automatically increase when the model is extended by an
  additional covariate. Usually they will, of course. A few (not too
  systematic) simulation studies for selected examples indicate that this
  happens in more than 99.9 of the cases, even when the added covariate
  has no effect at all. »
  p. 370-371 : « If this property is considered overwhelmingly important
  (as it seems to be, e.g., for Cameron and Windmeijer 1997), a
  likelihood-based R²-substitute should probably be preferred. »
  p. 371 : le maximum de Cox-Snell, 1 − exp(2l̂₀/n), « never exceeds
  0.75 ».
- Allison (2014), SAS Global Forum, Paper 1485-2014.
  p. 1-2 : « For years, I've been recommending the Cox-Snell R2 over the
  McFadden R2, but I've recently concluded that that was a mistake. I now
  believe that McFadden's R2 is a better choice. »
  p. 2 : la borne de Cox-Snell « reaches a maximum of .75 when p=.5 » ;
  sur Nagelkerke : « But this correction is purely ad hoc, and it greatly
  reduces the theoretical appeal of the original R2C&S. I also think that
  the values it typically produces are misleadingly high, especially
  compared with what you get from just doing OLS with the binary
  dependent variable. » ; sur Tjur : « But there's another R2, recently
  proposed by Tjur (2009), that I'm inclined to prefer over McFadden's. »
  Allison définit D comme la valeur absolue de la différence des deux
  moyennes (p. 2) ; p. 3, son exemple PROC TTEST soustrait les classes
  dans l'ordre 0 − 1, d'où « Ignoring the sign of the difference ».
  p. 3 : « it's possible that adding a variable to the model could reduce
  the Tjur R2 » ; « the Tjur R2 cannot be easily generalized to ordinal
  or nominal logistic regression. For McFadden and Cox-Snell, the
  generalization is trivial. »
  p. 1 : « SPSS reports the Cox-Snell measures for binary logistic
  regression but McFadden's measure for multinomial and ordered logit. »
- Menard (2000), *The American Statistician* 54(1). p. 20 : R²_L =
  1 − ln(L_M)/ln(L_0), la mesure de McFadden. p. 24 : « R²_L stands out
  for its independence from the base rate, relative to other R2 analogs,
  making it the most generally applicable and consistently useful of the
  R2 analogs. » p. 23 : seuls τp et R²_L ont une corrélation au carré avec
  le taux de base inférieure à 0,100 ; Nagelkerke en a une moindre que
  Cox-Snell, « but still substantial ».
- Mittlböck et Schemper (1996), *Statistics in Medicine* 15(19). p. 1991,
  sur la correction de Cragg et Uhler puis Nagelkerke : « the correction
  appears cosmetic as it can only force R²_LRU to 100 per cent for
  complete agreement and there is no indication why the scaling of the
  intermediate values of R²_LRU should be adequate. »
- Nagelkerke (1991), *Biometrika* 78(3). p. 692 : « R² thus defined
  achieves a maximum of less than 1 for discrete models, i.e. models whose
  likelihood is a product of probabilities, which have a maximum of 1,
  instead of densities, which can become infinite. » ; « For logistic
  regression, with 50% y = 1 and 50% y = 0 observations, this maximum
  equals 0·75. » La correction est proposée pour les modèles discrets.
  Même page, propriété (v) de Cox-Snell : « It is dimensionless, i.e. it
  does not depend on the units used. »
- Cameron et Windmeijer (1997), *Journal of Econometrics* 77(2).
  p. 332 : R²_KL = 1 − K(y, μ̂)/K(y, μ̂₀) (éq. 5), et Proposition 1 :
  « R²_KL is nondecreasing as regressors are added », « 0 ≤ R²_KL ≤ 1 ».
  p. 333 : « R²_KL equals the likelihood ratio index 1 − l(μ̂;y)/l(μ̂₀;y)
  if and only if l(y;y) = 0. » ; « Equality occurs for the Bernoulli
  model, but in general the likelihood ratio index differs and, for other
  discrete dependent variable models, is more pessimistic regarding the
  contribution of regressors, as l(y;y) ≤ 0. In the continuous case,
  large values (positive or negative) of the likelihood ratio index can
  arise if l(μ̂₀;y) is close to zero (positive or negative). By contrast,
  R²_KL will always be bounded by zero and one. » ; K(y, μ) de l'éq. 4
  « equals the definition of the deviance ».
  p. 335 : pour la loi normale, R²_KL « equals the usual coefficient of
  determination in the linear model » ; pour les modèles de Bernoulli,
  l(y;y) = 0 et R²_KL égale l'indice de McFadden. p. 336 : tableau 1
  (normale, Bernoulli, binomiale à m essais, Poisson, géométrique,
  exponentielle, Gamma, inverse gaussienne). p. 337 : pour la
  multinomiale aussi, l(y;y) = 0. p. 338 : avec un paramètre d'échelle
  inconnu (normale, Gamma), il se simplifie entre numérateur et
  dénominateur. p. 339, note 4 : pour la binomiale négative, Cameron et
  Windmeijer (1996) « propose setting the scale parameter to its estimate
  in the fitted model ». p. 340 : « In Bernoulli models, such as probit
  and logit, R²_KL coincides with the likelihood ratio index, supporting
  use of this index rather than the many other competing R² measures. »
- Long et Freese (2014), *Regression Models for Categorical Dependent
  Variables Using Stata*, 3e éd. p. 126 : « Because different pseudo-R2
  measures can yield substantially different results and different
  software packages use different measures as their default pseudo-R2,
  when presenting results it is important to report exactly which measure
  is being used rather than simply saying “Pseudo-R2”. » Même page :
  McFadden « is reported by Stata in the header of estimation results as
  Pseudo R2 ». p. 120 : « we often find these measures of limited utility
  in our own research, with the exception of the information criteria
  BIC and AIC. »
- Hemmert, Schons, Wieseke et Schimmelpfennig (2018), *Sociological
  Methods & Research* 47(3). PDF du magasin = version OnlineFirst
  (pagination 1-25). p. 10 : « a comparison of pseudo-R2 against a single
  benchmark value is generally not feasible. In particular, suggestions
  to simply transfer the benchmark values set by McFadden to any other
  pseudo-R2 [...] are unfounded. » p. 18 : « (iii) We urge researchers to
  report which pseudo-R2 has been used in their analyses » ; « (iv) For
  separate evaluations of logit models, we recommend the pseudo-R2 by
  Horowitz (1982) », 1 − (LL_V − m/2)/LL₀ (tableau 1, p. 6).
- Hosmer, Lemeshow et Sturdivant (2013), *Applied Logistic Regression*,
  3e éd. p. 184 : R²_L = 1 − L_p/L₀, « pseudo R² in STATA », que
  Mittlböck et Schemper « do not recommend [...] for routine use, as it is
  not as intuitively easy to explain » ; calculé p. 185 sur les n
  observations individuelles (« assuming J = 500 »), où la vraisemblance
  saturée vaut 0. p. 185 : CD = π̂̄₁ − π̂̄₀ (éq. 5.20) ; « low R² values in
  logistic regression are the norm ».
- Harrell (2015), *Regression Modeling Strategies*, 2e éd. p. 206-207 :
  R²_N de Nagelkerke, « This is the form of the R2 index we use
  throughout » (rms l'imprime pour `lrm`). p. 257 : D de Tjur « has many
  advantages as an index of predictive power ». p. 272, note 12 : Menard
  « criticized R²_N for being too dependent on the proportion of
  observations with Y = 1 ».

## Mesures

Scripts dans le scratchpad de la session, hors du dépôt. R 4.6.2,
performance 0.19.0, DescTools 0.99.60, pscl 1.5.9, rms 8.2.1, MASS
7.3-66, nnet 7.3-21, statmod 1.5.2.

1. Monotonie. Un prédicteur de pur bruit est ajouté à y ~ x1 + x2
   (logit), 2 000 à 4 000 réplications par cellule, n de 30 à 1 000 et
   deux tailles d'effet. Tjur baisse dans 2,4 % à 8,0 % des ajouts, de
   0,015 au plus, la médiane des baisses ne dépassant pas 4e-4. McFadden,
   Cox-Snell et Nagelkerke ne baissent jamais. Sur des sous-échantillons
   de 80 personnes de `sochealth`, Tjur baisse dans 8,1 % des ajouts d'un
   bruit. Les simulations de Tjur (p. 370, « not too systematic »)
   annonçaient plus de 99,9 % de hausses. Nous en trouvons 92 à 97,6 %.
2. Taux de base. Mêmes pentes latentes (logit, b = 1 et 0,5), constante
   réglée pour une proportion d'événements de 0,50 puis 0,05, n = 200 000 :
   Tjur passe de 0,205 à 0,073, McFadden de 0,162 à 0,133, Cox-Snell de
   0,201 à 0,051 et Nagelkerke de 0,268 à 0,157. Le R² latent de
   McKelvey et Zavoina vaut 0,276.
3. Regroupement. Les mêmes 300 essais, en 30 lignes `cbind()` de 10
   essais puis en 300 lignes 0/1, donnaient McFadden 0,275 contre 0,095,
   Nagelkerke 0,735 contre 0,164 et Tjur absent contre 0,124. Les autres
   packages ne font pas mieux. performance 0.19.0 renvoie NULL avec un
   avertissement pour `cbind()` et pour une proportion pondérée. Sur des
   lignes 0/1 pondérées par des effectifs, il ignore les poids dans
   Cox-Snell, Nagelkerke (0,60 contre 0,22 sur les données développées)
   et Tjur (0 au lieu de 0,18). DescTools donne pour le `cbind()`
   McFadden 0,93, Cox-Snell et Nagelkerke 1, Tjur −0,004. pscl::pR2
   échoue (portée des données).
4. Unité de y, familles continues (n = 200, y multiplié par 0,001, 0,01,
   0,1, 1, 10 et 1 000). Pour une Gamma à lien log, McFadden vaut −0,599
   à y/10, 0,092 à y et 0,021 à 1 000 y. Nagelkerke est NA jusqu'à y/10
   (borne ≤ 0), puis 0,312 à y et 0,306 à 1 000 y. Le R² de déviance
   vaut 0,290 à toutes les échelles. L'inverse gaussienne à lien log et
   la gaussienne à lien identité ou log donnent le même tableau. Pour la
   gaussienne identité, le R² de déviance (0,437) est le R² de `lm()`,
   que Cox-Snell rejoint. Avant le lot, un `glm` gaussien identité
   affichait McFadden 0,20 et Nagelkerke 0,52 pour un R² de 0,50.
5. Poids non entiers (logit, n = 300, poids uniformes entre 0,2 et 3).
   `logLik()` d'un `glm` binomial arrondit les poids : −281,2, contre
   −276,1 pour la log-vraisemblance pondérée que `glm()` maximise. Avec
   des poids multipliés par 10, DescTools passe de McFadden 0,158 à
   0,147 et de Nagelkerke 0,332 à 0,958, performance de Nagelkerke 0,308
   à 0,958 (son McFadden échoue alors, faute de retrouver les poids). La
   règle retenue donne 0,147, 0,246 et 0,192 (McFadden, Nagelkerke, Tjur)
   aux deux échelles.

## Décisions

1. Défauts. Un logit sur une réponse binaire (0/1, facteur ou logique,
   avec ou sans poids) affiche Tjur puis McFadden. Les autres binomiaux
   (probit, cloglog, réponse groupée), Poisson, la binomiale négative,
   `polr`, `clm`, `multinom` et `rms::lrm` affichent McFadden seul.
   `rms::Glm` suit son jumeau `glm`, et Tjur lui est désormais calculé.
   Nagelkerke quitte le défaut et reste un jeton. Les classes sans
   pseudo-R² par défaut (mixtes, `svyglm`, `coxph`, `survreg`,
   `betareg`, `gam`, `mlogit`, bayésiennes, `fixest`) ne changent pas.
   Un tableau de plusieurs modèles affiche l'union des lignes, dans
   l'ordre Tjur, McFadden, déviance. Une cellule porte le tiret quand la
   mesure n'est pas définie pour son modèle et la valeur quand elle
   l'est, même hors du défaut de ce modèle : le Tjur d'un probit voisin
   d'un logit, le R² de déviance d'un Poisson voisin d'une Gamma.
2. Essais individuels. Pour un binomial, McFadden, Cox-Snell et
   Nagelkerke sont calculés sur la vraisemblance de Bernoulli des
   essais, avec n le nombre total d'essais (la somme des poids a
   priori), et Tjur sur des moyennes pondérées par les essais. Un modèle
   groupé rend exactement les nombres du modèle développé (oracle :
   réajustement sur les lignes 0/1, écart ≤ 1e-10). Aucune source ne le
   prescrit pour `cbind()`, et Cameron et Windmeijer donnent même une
   divergence groupée pour la binomiale à m essais (tableau 1). La
   décision tient à l'invariance et au fait que les bornes citées (le
   maximum 0,75 de Cox-Snell à 50 %, chez Nagelkerke p. 692, Allison
   p. 2, Tjur p. 371, Mittlböck et Schemper p. 1991) comme la définition
   de D (Tjur p. 367, « proper binary data, without aggregation to
   binomial counts ») supposent la vraisemblance de Bernoulli des
   observations individuelles.

   Avec des poids non entiers, les mêmes formules sont la
   log-vraisemblance pondérée que `glm()` maximise, avec n la somme des
   poids. Toutes les mesures deviennent invariantes à l'échelle des
   poids, ce que donne l'option NORMALIZE de SAS (documentation
   « Generalized Coefficient of Determination » de PROC LOGISTIC :
   n = Σ f_j n_j, « Specifying the NORMALIZE option in the WEIGHT
   statement makes these coefficients invariant to the scale of the
   weights »). Des poids proportionnels à des effectifs, normalisés à
   une moyenne de 1, rendent les valeurs des données développées. La
   même règle de n vaut pour Poisson, la binomiale négative, `multinom`
   et le `lrm` binaire, qui prenaient le nombre de lignes. `polr`, `clm`
   et le `lrm` ordinal prenaient déjà la somme des poids.

   Tjur garde son signe. La définition est une différence de moyennes
   (Tjur p. 369), positive pour un logit avec constante et sans offset
   (Corollaire 1). Un offset fixe peut la rendre négative : −0,24 pour un
   modèle de la seule constante avec l'offset −3x. La valeur absolue
   d'Allison (p. 2) ne sert qu'à ignorer l'ordre de soustraction de PROC
   TTEST (p. 3). Tjur reste calculé pour le probit et le cloglog, en
   option, et la documentation dit que l'égalité des moyennes est propre
   au logit (Tjur p. 367).
3. Familles continues. Pour la gaussienne (tous liens), la Gamma,
   l'inverse gaussienne et la Tweedie, le R² de déviance 1 − D/D₀
   (Cameron et Windmeijer, éq. 5) remplace McFadden et Nagelkerke au
   défaut. Ces deux jetons sont refusés (`spicy_invalid_input`) quand
   tous les modèles du tableau sont de ces familles et portent le tiret
   à côté d'autres modèles. La gaussienne identité suit la même règle.
   Elle affichait McFadden et Nagelkerke, pas le R² (mesure 4), et son R²
   de déviance est le R² de `lm()` (égalité vérifiée à 1e-12, pondérée ou
   non).

   Pour les familles de comptage, McFadden reste le défaut et le R² de
   déviance est en option. Pour `MASS::glm.nb`, D et D₀ prennent tous
   deux le θ du modèle complet, la proposition de Cameron et Windmeijer
   (1996) rappelée p. 339, note 4. Le jeton lui est donc offert.
   Précisément, spicy prend le θ de `family(fit)`, celui de la dernière
   itération des moindres carrés repondérés de `glm.nb()`, qui écrit
   ensuite une dernière mise à jour dans `fit$theta`. Les deux ne
   diffèrent que de la tolérance de convergence (garde : écart au calcul
   avec `fit$theta` ≤ 1e-6). Sans offset, le résultat est exactement
   `1 - deviance / null.deviance` de MASS. Avec un offset, MASS réajuste
   son modèle nul avec le θ mis à jour, d'où le même écart de
   convergence.

   Les familles quasi ont une déviance sans vraisemblance. Le R² de
   déviance leur est offert en option, pas au défaut. Pour le binomial,
   le R² de déviance sur les essais égale McFadden (l(y;y) = 0, p. 333 et
   340). Le jeton y est refusé avec un message qui renvoie à McFadden,
   plutôt que d'afficher deux lignes du même nombre sous deux noms. Il
   en va de même pour `polr`, `clm`, `multinom` et `lrm` (multinomiale,
   p. 337). Le quasi-binomial le reçoit, calculé sur les essais.

## Alternatives écartées

- Tjur seul pour le logit. Il peut baisser quand on ajoute un prédicteur
  (mesure 1, Allison p. 3) et dépend davantage du taux de base que
  McFadden (mesure 2, Menard p. 24). Il ne se généralise pas à
  l'ordinal (Allison p. 3), et Tjur lui-même conseille une mesure fondée
  sur la vraisemblance quand la monotonie compte (p. 370-371). Le couple
  garde la lecture sur l'échelle des probabilités et la mesure monotone.
- Le statu quo (McFadden et Nagelkerke). Nagelkerke est « purely ad
  hoc » et « misleadingly high » (Allison p. 2), sa correction
  « cosmetic » (Mittlböck et Schemper p. 1991). Elle a été conçue pour
  les modèles discrets (Nagelkerke p. 692), et la mesure dépend du taux
  de base (Menard p. 23, Harrell p. 272).
- McFadden seul pour le logit. Défendable (Allison p. 1-2, Menard p. 24,
  Cameron et Windmeijer p. 340), mais Tjur (p. 366 et 370) et Allison
  (p. 2) recommandent D pour le logit binaire, que performance donne par
  défaut.
- La variante ajustée de Horowitz, recommandée par Hemmert et al.
  (p. 18) pour comparer des modèles et pour leurs seuils. Aucun logiciel
  de référence ne l'imprime, et ses seuils, tirés d'une méta-analyse de
  modèles binaires, ne valent pas pour les autres familles.
- Le R² de déviance partout. Pour le binomial et le multinomial il égale
  McFadden. Ailleurs dans le discret, McFadden est le « Pseudo R2 » de
  Stata et le plus pessimiste des deux (p. 333) : il reste le défaut.

## Ce qui rouvrirait la question

Une source qui établisse pour Tjur une monotonie ou une indépendance au
taux de base meilleures que celles mesurées ici. Une convention de
Stata, SAS ou SPSS qui calcule les pseudo-R² d'un modèle groupé sur les
lignes plutôt que sur les essais (SAS fait l'inverse). Une demande
d'utilisateur pour Horowitz ou Cox-Snell en jeton.

## Pratiques des logiciels

- Stata : McFadden, le « Pseudo R2 » de l'en-tête des résultats
  d'estimation (Long et Freese p. 126). Le manuel [R] maximize le
  définit : « The pseudo-R2 (McFadden 1974) is defined as 1 − L1/L0 ».
  Les exemples des entrées `poisson` et `ologit` du manuel l'impriment.
- SAS PROC LOGISTIC, option RSQUARE : Cox-Snell (« RSquare ») et
  Nagelkerke (« Max-rescaled RSquare »), avec n = Σ f_j n_j, donc le
  nombre d'essais en syntaxe événements/essais.
- SPSS LOGISTIC REGRESSION : Cox-Snell et Nagelkerke (IBM SPSS
  Statistics 29 Algorithms, « Goodness of Fit »). PLUM et NOMREG
  ajoutent McFadden (Allison p. 1).
- R : performance::r2() donne Tjur pour un logit et Nagelkerke pour le
  probit, Poisson et Gamma (vérifié avec la 0.19.0). rms::lrm imprime
  Nagelkerke (Harrell p. 206-207).

## Garde

`tests/testthat/test-pseudo_r2.R` : oracles sur les données développées
(`cbind`, proportion pondérée, lignes 0/1 pondérées par des effectifs),
poids non entiers (formule pondérée, invariance à l'échelle), Poisson,
binomiale négative, `multinom` et `lrm` pondérés, signe de Tjur avec un
offset, `na.exclude`, `y = FALSE`, R² de déviance
(`1 - deviance/null.deviance`, invariance d'unité, `glm.nb`, quasi,
Tweedie, gaussienne égale au R² de `lm()`), refus classés.
`tests/testthat/test-fit-stats-defaults.R` : les lignes par défaut de
chaque classe, les tableaux mixtes, `nested = TRUE`,
`show_columns = "none"` et le français.
