# ordinal : clmm() permute les liens cloglog et loglog (switch C)

- **Cible** : ordinal 2026.7-26 (CRAN et GitHub main 05fe6075 identiques)
- **Canal** : issue GitHub, <https://github.com/runehaubo/ordinal/issues> ;
  aucune issue existante (recherches « cloglog », « loglog », « clmm link »
  le 2026-10-09, 17 issues ouvertes, aucune liée) ; le NEWS ne mentionne
  qu'un « Bug in loglog-link for clmm fits fixed » de 2010 (ancien code).
- **Envoyé** : 2026-10-09, issue [#76](https://github.com/runehaubo/ordinal/issues/76)
- **Statut** : ouvert
- **Côté spicy** : regression_frame_ordinal.R, méthode clmm : `exponentiate`
  refusé sous cloglog, titre sans « proportional hazards »
- **Vérifications, toutes refaites par moi le 2026-10-09** : chaque
  numéro de ligne lu dans le tarball CRAN ; chaîne d'appel suivie dans
  le code (clmm.ssr.R → NRalgv3 ligne 252, getNAGQ 199, getNGHQ_C 165 →
  d_nll 387-391, grad_C 431-432, hess 491-492, getNAGQ 800-804,
  getNGHQ_C 750-754 → switches) ; vignette clm_article.Rnw ligne 532
  (log-log = Gumbel(max), c-log-log = Gumbel(min)) ; gumbel.R lignes
  29 et 38 (« loglog link » / « cloglog link ») ; pgumbel2_C = 1 −
  d_pgumbel(−q) (utilityFuns.c 61-67) ; snippet de vraisemblance exécuté
  tel quel ; patch appliqué à la source CRAN, installé dans une
  bibliothèque temporaire, refits sur wine ; clmm2() (clm2.R 97-104,
  même linkInt 3/4, mêmes routines C) touché de la même façon et
  corrigé par le même patch (CRAN : cloglog −82,7294, loglog −81,5414 ;
  patché : l'inverse). Tous les appelants R des routines C passent ce
  même entier : aucun ne dépend du croisement. Arrondis à trois
  décimales, deux pour les écarts-types des juges (0,791 clmm,
  0,792 glmmTMB).

---

**Title:** clmm(): the cloglog and loglog links are swapped in the C link functions

In ordinal 2026.7-26, `clmm(link = "cloglog")` fits the loglog model and
`clmm(link = "loglog")` fits the cloglog model. `clm()` is not affected.
Line numbers are those of the CRAN source of 2026.7-26; R 4.6.1.

Where. `R/clmm.ssr.R` (lines 41-46) passes `linkInt` 3 for cloglog and 4
for loglog to the C code. In `src/utilityFuns.c`, the four link switches
`d_pfun` (192-195), `d_pfun2` (221-224), `d_dfun` (256-259) and `d_gfun`
(286-289), which the clmm likelihood, gradient and Hessian use, return
the `*gumbel` functions for case 3 ("cloglog") and the `*gumbel2`
functions for case 4 ("loglog"). But `d_pgumbel` is the Gumbel(max) CDF
`exp(-exp(-q))` (`src/links.c` 29-43), the loglog link, and `d_pgumbel2`
the Gumbel(min) CDF `1 - exp(-exp(q))` (46-60), the cloglog link, as
`R/gumbel.R` itself says (`pgumbel(max = TRUE)` is commented "loglog
link", `max = FALSE` "cloglog link") and as `clm()` uses them
(`R/utils.R` 29-32). The two cases are crossed. `clmm2()` passes the
same integers (`R/clm2.R` 97-104) and is affected the same way.

On `wine`:

```r
library(ordinal)
data(wine)
f  <- rating ~ temp + contact
fr <- rating ~ temp + contact + (1 | judge)
logLik(clm(f, data = wine, link = "cloglog"))   # -86.634
logLik(clm(f, data = wine, link = "loglog"))    # -87.718
logLik(clmm(fr, data = wine, link = "cloglog")) # -82.729
logLik(clmm(fr, data = wine, link = "loglog"))  # -81.541
```

The two `clm()` values are the log-likelihoods recomputed from the
fitted parameters with P(Y <= j) = F(theta_j - x'b):

```r
ll_hand <- function(fit, F) {
  X <- model.matrix(f, wine)[, -1, drop = FALSE]
  eta <- drop(X %*% fit$beta)
  theta <- c(-Inf, fit$alpha, Inf)
  y <- as.integer(wine$rating)
  sum(log(F(theta[y + 1] - eta) - F(theta[y] - eta)))
}
cll <- clm(f, data = wine, link = "cloglog")
ll  <- clm(f, data = wine, link = "loglog")
ll_hand(cll, function(z) 1 - exp(-exp(z)))  # -86.634, as logLik(cll)
ll_hand(ll,  function(z) exp(-exp(-z)))     # -87.718, as logLik(ll)
```

glmmTMB 1.1.15.2, whose ordinal family has a cloglog link, gives
-86.634 without random effects, the `clm()` value, and -81.541 with the
judge intercept, the value of `clmm(link = "loglog")`. Its slopes (2.047
and 1.225), judge SD (0.79) and thresholds (-1.787, 0.530, 2.357, 3.492)
are those of `clmm(link = "loglog")`, not those of
`clmm(link = "cloglog")` (1.973, 1.128 and 0.76).

Fix. Swap the Gumbel calls of cases 3 and 4 in the four switches of
`src/utilityFuns.c`:

```diff
     case 3: // cloglog
-	return d_pgumbel(x, mu, sigma, lower_tail);
+	return d_pgumbel2(x, mu, sigma, lower_tail);
     case 4: // loglog
-	return d_pgumbel2(x, mu, sigma, lower_tail);
+	return d_pgumbel(x, mu, sigma, lower_tail);
```

The same in `d_pfun2`; `d_dgumbel` and `d_dgumbel2` in `d_dfun`;
`d_ggumbel` and `d_ggumbel2` in `d_gfun`. With these eight lines applied
to the CRAN source and installed, `clm()` is unchanged, `clmm2()` is
corrected the same way, and:

```r
logLik(clmm(fr, data = wine, link = "cloglog")) # -81.541
logLik(clmm(fr, data = wine, link = "loglog"))  # -82.729
```

with, for the cloglog fit, slopes 2.047 and 1.225, judge SD 0.79 and
thresholds -1.787, 0.530, 2.357, 3.492: the glmmTMB values.
