# parameters : standardize_parameters(method = "posthoc") ne divise pas par SD(x) pour brmsfit

- **Cible** : easystats/parameters (via effectsize)
- **Canal** : GitHub, easystats/parameters#1243, https://github.com/easystats/parameters/issues/1243
- **Envoyé** : 2026-07-22
- **Statut** : ouvert au 2026-09-06
- **Côté spicy** : gate Bayes dans tests/testthat/test-stan_bayes_gates_re.R (commentaire renvoyant ici)

---

*Dossier tel qu'envoyé, conservé verbatim ci-dessous.*

# POSTEE 2026-07-22: https://github.com/easystats/parameters/issues/1243

**Titre:** `standardize_parameters(method = "posthoc")` does not scale continuous predictors by SD(x) for brmsfit models

**Corps:**

For the same model and data, `standardize_parameters(method = "posthoc")` produces different conventions depending on the Bayesian backend: the `stanreg` path scales continuous coefficients by SD(x) as documented, the `brmsfit` path does not.

```r
library(brms); library(rstanarm); library(effectsize)
d <- lme4::sleepstudy
d$grp <- factor(rep(c("a", "b", "c"), 60))

set.seed(1)
bf <- brm(Reaction ~ Days + grp, data = d, chains = 1, iter = 600,
          backend = "rstan", refresh = 0)
set.seed(1)
sf <- stan_glm(Reaction ~ Days + grp, data = d, chains = 1,
               iter = 600, refresh = 0)

standardize_parameters(sf, method = "posthoc")
#> Days ~ 0.536  == median(Days draws) * sd(Days) / sd(Reaction)   [expected]
standardize_parameters(bf, method = "posthoc")
#> b_Days ~ 0.185 == median(b_Days draws) / sd(Reaction)           [sd(Days) missing]
```

The ratio between the two (unrounded) outputs is exactly `sd(Days) = 2.88`. `method = "basic"` agrees between the two backends (and with the algebra), so the discrepancy is specific to the posthoc path for `brmsfit`.

The mechanism seems to be in `standardize_info()`: for a `brmsfit`, `Deviation_Smart` is 1 for continuous predictors while `Deviation_Basic` correctly finds SD(x), and `.standardize_posteriors_posthoc()` takes its predictor deviation from the `Deviation_Smart` column — so the posthoc factor collapses to 1/SD(y). Minimal probe (a smaller model, to keep the output short):

```r
set.seed(1)
bf0 <- brm(Reaction ~ Days, data = lme4::sleepstudy, chains = 1,
           iter = 400, backend = "rstan", refresh = 0)
parameters::standardize_info(bf0)[, c("Parameter", "Deviation_Smart", "Deviation_Basic")]
#>     Parameter Deviation_Smart Deviation_Basic
#> 1 b_Intercept               0        0.000000
#> 2      b_Days               1        2.880293   <-- Smart expected to be 2.88 too
#> 3       sigma               1              NA

set.seed(1)
sf0 <- rstanarm::stan_glm(Reaction ~ Days, data = lme4::sleepstudy,
                          chains = 1, iter = 400, refresh = 0)
parameters::standardize_info(sf0)[, c("Parameter", "Deviation_Smart", "Deviation_Basic")]
#>     Parameter Deviation_Smart Deviation_Basic
#> 1 (Intercept)        0.000000        0.000000
#> 2        Days        2.880293        2.880293
```

The `stanreg` parameter is named `Days` and gets `Deviation_Smart = 2.88`; the `brmsfit` parameter is named `b_Days` and falls back to 1, so the `b_` prefix may be what breaks the smart-deviation lookup.

Expected: identical standardization for identical models regardless of backend. Happy to provide more probes if useful.

Versions: parameters 0.29.2, effectsize 1.0.3, insight 1.5.2, datawizard 1.3.1, brms 2.23.0, rstanarm 2.32.2, R 4.6.1, Windows 11.

(Found while cross-validating standardized coefficients in a downstream table package; we use `method = "basic"` as the oracle there since it is backend-invariant.)
