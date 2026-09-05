# merDeriv : vcov.lmerMod(full = TRUE) à coût cubique en n

- **Cible** : merDeriv
- **Canal** : GitHub, nctingwang/merDeriv#10, https://github.com/nctingwang/merDeriv/issues/10
- **Envoyé** : 2026-07-22
- **Statut** : ouvert au 2026-09-06
- **Côté spicy** : erreurs standard des composantes de variance des modèles mixtes

---

*Dossier tel qu'envoyé, conservé verbatim ci-dessous.*

# POSTEE 2026-07-23: https://github.com/nctingwang/merDeriv/issues/10

**Titre:** vcov.lmerMod(full = TRUE) cost grows roughly cubically with n — intractable beyond a few thousand observations

**Corps:**

Thank you for merDeriv — [spicy](https://github.com/amaltawfik/spicy) (a downstream table package) uses `vcov.lmerMod(full = TRUE, ranpar = "var")` to report standard errors of random-effect variance components for `lmer` fits.

The cost of that call grows roughly cubically with the number of observations, which makes it intractable beyond a few thousand rows. Wall-clock times measured on 2026-07-22 (merDeriv 0.2-6) for a random-intercept model `MathAch ~ SES + (1 | School)` on subsamples of the High School & Beyond data shipped with nlme (`nlme::MathAchieve`, 160 schools):

| n (observations) | groups | time |
|---|---|---|
| 820 | 20 | 1.5 s |
| 2,626 | 60 | 50 s |
| 7,185 | 160 | stopped at a 10-min cap; an earlier uncapped run reached 40+ min and ~1.5 GB RSS before we gave up |

The scaling between the first two points is approximately cubic: log(50/1.5) / log(2626/820) = 3.0, and the third point is consistent with that rate. `information = "expected"` is not cheaper (52 s at n = 2,626), and a random-slope model at the same size takes 208 s.

Reproducible example:

```r
library(lme4)
hsb <- as.data.frame(nlme::MathAchieve)
hsb$School <- factor(hsb$School)
set.seed(1)
keep <- sample(levels(hsb$School), 60)
h60 <- droplevels(hsb[hsb$School %in% keep, ])   # 2,626 rows
m <- lmer(MathAch ~ SES + (1 | School), data = h60)
system.time(merDeriv::vcov.lmerMod(m, full = TRUE, ranpar = "var"))
```

For comparison, the same quantity is essentially free elsewhere because it is a fit-time Hessian byproduct rather than a per-observation reconstruction: `nlme::intervals()` reads `apVar` in ~0 s (below timer resolution) at n = 7,185, and `TMB::sdreport()` on the equivalent `glmmTMB` fit takes 0.13 s at the same size.

A few directions that might help, in case any of them fits merDeriv's design (you know constraints we do not — the per-observation scores serve other purposes too):

1. **Blocked per-cluster assembly.** With independent clusters, V = ZGZ' + sigma^2 I is block-diagonal by group, so the information matrix decomposes into per-cluster contributions with small dense solves (n_j x n_j) — cost linear in the number of groups — without ever materializing an n x n matrix.
2. **Assembly from lme4's sparse objects.** `getME(m, "Z")`, `Lambdat` and the Cholesky factor `L` are already sparse; the (beta, theta, sigma^2) information can be built from them with sparse products.
3. **An opt-in cheaper route for the variance-component block only.** A numerical Hessian of the profiled deviance at the estimate (`lme4::devfun2`-style, which is conceptually what `nlme` stores in `apVar` and `glmmTMB` in `sdreport()`) would give the block downstream users typically need; the fixed-effect block is already available from `vcov()`.

Happy to provide profiling output, and to run a development branch against this benchmark.

Environment: R 4.6.1, Windows 11, merDeriv 0.2-6, lme4 2.0.6, nlme 3.1-170, glmmTMB 1.1.14.
