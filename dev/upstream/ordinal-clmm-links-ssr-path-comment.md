# ordinal : commentaire sur #76, la permutation ne touche que le chemin à intercept aléatoire unique

- **Cible** : ordinal 2026.7-26 (CRAN), R 4.6.2, glmmTMB 1.1.15.2
- **Canal** : commentaire sur l'issue [#76](https://github.com/runehaubo/ordinal/issues/76), déposée le 2026-10-09 (dossier [ordinal-clmm-cloglog-loglog-links.md](ordinal-clmm-cloglog-loglog-links.md))
- **Envoyé** : 2026-11-10, <https://github.com/runehaubo/ordinal/issues/76#issuecomment-6771681112>
- **Statut** : ouvert, aucune réponse du mainteneur à l'issue ni au commentaire au 2026-11-10
- **Côté spicy** : `.clmm_ssr_path()` et `.clmm_links_swapped()` (R/regression_frame_ordinal.R) appliquent la même restriction : le lien n'est tenu pour permuté que sur ce chemin, et seulement si une détection faite une fois par session le constate ; test-regression_frame_ordinal.R couvre cinq structures aléatoires contre glmmTMB
- **Vérifications, refaites le 2026-11-10 avant l'envoi** : seize ajustements (quatre structures aléatoires, deux liens, avec et sans `useMatrix`, `nAGQ` 5 et −5) contre glmmTMB et `clm()` ; chaîne d'appel suivie dans le tarball CRAN (`use.ssr` clmm.R 68 et 255 ; `linkInt` passé au C par NRalgv3, getNAGQ, getNGHQ_C de clmm.ssr.R et par clmm2 ; chemin général nll.u → getFittedC → get_fitted.c 76-95) ; patch de huit lignes appliqué à la source CRAN et installé en bibliothèque temporaire, refits comparés au build CRAN (chemin général et `clm()` identiques au dernier chiffre) ; `clm2(link = "loglog")` relancé sur les deux builds, `switch` de clm2.R 87-95 lu dans la source

## Commentaire tel qu'envoyé

The swap only happens on the single-random-intercept path. I fitted the
other random structures (ordinal 2026.7-26, R 4.6.2, glmmTMB 1.1.15.2).
`clmm()` takes that path for one bar term with `1` on its left and
`useMatrix = FALSE`, the default (`use.ssr`, `R/clmm.R` 68 and 255),
with Laplace or quadrature. Only that path, and `clmm2()`, passes
`linkInt` to the switches of `src/utilityFuns.c` (through `NRalgv3`,
`getNAGQ` and `getNGHQ_C`). Every other fit goes through `nll.u()`
(`R/clmm.R` 415), `getFittedC()` (`R/utils.R` 156) and `get_fitted()`
(`src/get_fitted.c` 76-95), which map `"cloglog"` to `d_pgumbel2` as
`clm()` does. So `useMatrix = TRUE`, two random terms, nested terms and
random slopes fit the link they name, and the same model fits
differently depending on the path:

```r
library(ordinal)
library(glmmTMB)
fr <- rating ~ temp + contact + (1 | judge)
ll <- function(f, link, ...)
  round(as.numeric(logLik(clmm(f, data = wine, link = link,
    control = clmm.control(...)))), 3)
ll(fr, "cloglog")                    # -82.729  single-intercept path
ll(fr, "loglog")                     # -81.541
ll(fr, "cloglog", useMatrix = TRUE)  # -81.541  general path
ll(fr, "loglog",  useMatrix = TRUE)  # -82.729
# a random slope takes the general path without useMatrix
fs <- rating ~ temp + contact + (1 + temp | judge)
ll(fs, "cloglog")                    # -81.444
round(as.numeric(logLik(glmmTMB(fs, data = wine,
  family = ordinal("cloglog")))), 3) # -81.444
```

`(1 | judge) + (1 | bottle)`, `(1 | judge/temp)` and `(1 | judge/bottle)`
also match glmmTMB within 1e-6 for both links. `nAGQ = 5` swaps like the
default. The first sentence of the issue holds for a single random
intercept with `useMatrix = FALSE` only; `useMatrix = TRUE` is a
workaround until the fix.

I applied the eight-line patch to the CRAN source and installed it. The
single-intercept fits swap back (-81.541 for cloglog, -82.729 for
loglog, with the glmmTMB slopes, SD and thresholds), `clmm2()` with
them. `clm()` and the general-path fits are unchanged to the last digit.

Separately, `clm2(rating ~ temp + contact, data = wine, link =
"loglog")` stops with `could not find function "gfun"`, on the CRAN
build and with the patch. The `switch` that sets `rho$gfun` (`R/clm2.R`
87-95) has two `cloglog` entries (lines 90-91) and no `loglog` entry.
