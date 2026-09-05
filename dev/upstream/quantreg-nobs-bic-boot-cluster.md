# quantreg : méthode nobs() absente (BIC = NA) et convention de longueur du cluster bootstrap

- **Cible** : quantreg 6.1
- **Canal** : courriel au mainteneur, faute de tracker public
- **Envoyé** : 2026-07-23
- **Statut** : envoyé, sans réponse connue au 2026-09-06
- **Côté spicy** : regression_frame_quantreg_AER.R, erreurs standard rq

---

*Dossier tel qu'envoyé, conservé verbatim ci-dessous.*

# ENVOYE PAR AMAL 2026-07-23 au mainteneur

Canal: courriel — quantreg n'a ni BugReports ni depot GitHub avec issues
(github.com/cran/quantreg est un miroir en lecture seule).
Relu mot a mot et re-verifie le 2026-07-23 sur quantreg 6.1 / R 4.6.1 :
messages d'erreur captures en locale anglaise, extrait de source verbatim,
comportements sandwich/clubSandwich testes empiriquement (les deux
acceptent les deux longueurs). A envoyer par Amal depuis sa boite.

---

**Subject:** quantreg: two small reports — nobs() method missing (stats::BIC returns NA) and summary.rq boot-cluster length convention

Dear Professor Koenker,

Thank you for quantreg. While wiring quantile regression into a
downstream table package (spicy), we hit two small rough edges that
seemed worth reporting. Both verified on quantreg 6.1, R 4.6.1.

**1. No nobs() method for rq objects — and stats::BIC() silently
returns NA**

`stats::nobs()` errors on rq fits, which breaks generic downstream
code (information-criteria helpers, cluster-length validation, any
`vapply(models, nobs, ...)` over mixed model sets):

```r
library(quantreg)
data(engel)
f <- rq(foodexp ~ income, data = engel, tau = 0.5)
nobs(f)
#> Error in nobs.default(f) : no 'nobs' method is available
```

A concrete casualty inside base R itself: `logLik.rq` sets the `df`
attribute but not `nobs`, so `stats::BIC(f)` quietly returns `NA`
(while `AIC(f)` works through your own `AIC.rq`):

```r
AIC(f)
#> [1] 2827.26
BIC(f)
#> [1] NA
```

The residual vector carries the answer, so a one-line method would
close both gaps:

```r
nobs.rq <- function(object, ...) length(object$residuals)
```

(optionally also setting the `nobs` attribute in `logLik.rq`, which
fixes `BIC()` directly).

**2. summary.rq(se = "boot", cluster = ) accepts the cluster vector
only at the original data length, and this is undocumented**

In the bootstrap branch, when `cluster` is supplied and the fit
dropped rows via `na.omit`, the cluster vector is subset by
`object$na.action` before reaching `boot.rq()` (from summary.rq's
source):

```r
if (length(object$na.action)) {
    cluster <- dots$cluster[-object$na.action]
    bargs <- modifyList(bargs, list(cluster = cluster))
}
```

So the API expects `cluster` at the ORIGINAL data length (pre-NA
drop). Elsewhere in the robust-vcov ecosystem this is forgiving —
`sandwich::vcovCL` and `clubSandwich::vcovCR` both accept either
length (subsetting by `na.action` themselves when given the original
one) — so a caller who passes a vector aligned with the rows the fit
actually used gets a hard stop:

```r
library(quantreg)
data(engel)
eng2 <- engel
eng2$foodexp[c(3, 7)] <- NA
eng2$cl <- rep(1:47, each = 5)
f <- rq(foodexp ~ income, data = eng2, tau = 0.5)   # 233 rows used

## aligned with the 233 fitted rows:
cl_fit <- eng2$cl[-f$na.action]
summary(f, se = "boot", R = 50, cluster = cl_fit)
#> Error in (function (x, y, tau = 0.5, R = 200, ... : cluster is wrong length
## works only with the full 235-length vector:
summary(f, se = "boot", R = 50, cluster = eng2$cl)  # OK
```

Neither `?summary.rq` nor `?boot.rq` documents which length is
expected. Either of two low-cost resolutions would help downstream
authors:

1. document the original-length expectation in `?summary.rq`, or
2. accept both lengths, as `sandwich::vcovCL` and
   `clubSandwich::vcovCR` do: subset only when `length(cluster)`
   matches the original n, pass through when it matches the fitted
   n, and stop with an explicit message otherwise.

(We currently work around it by calling `boot.rq()` directly on x/y
rebuilt from the fitted model frame.)

With best regards, and thanks again for quantreg,

Amal Tawfik
spicy, https://github.com/amaltawfik/spicy

---

## Reponse de Koenker (recue 2026-07-28)

nobs.rq ajoute dans sa version de developpement. Demande de
clarification sur le point cluster: il cite ?boot.rq, qui documente
BEL ET BIEN la longueur d'origine ("the length of the 'cluster'
variable should always be the same as the length of the original
response variable before any 'na.action' takes place").

VERIFIE (quantreg 6.1 installee 2026-04-24, empaquetee 2025-02-28,
donc AVANT notre email): la phrase etait deja dans ?boot.rq. Notre
affirmation "Neither ?summary.rq nor ?boot.rq documents which length
is expected" etait fausse pour ?boot.rq (vraie pour ?summary.rq, ou
cluster passe par `...`). A conceder proprement.

VERIFIE aussi: nobs.rq seul repare BIC() -- stats:::BIC.default
retombe sur nobs(object) quand logLik n'a pas l'attribut nobs.
BIC(f) = 2834.179 sur engel. Aucun changement a logLik.rq necessaire.

VERIFIE (source summary.rq): dots (cluster compris) entre entierement
dans bargs via modifyList; le bloc na.action ne fait que REMPLACER
cluster par sa version tronquee. Conditionner ce remplacement a la
longueur d'origine = pass-through exact pour la longueur ajustee.

## Verification claim-par-claim de la reponse (2026-07-28)

Chaque affirmation du brouillon ci-dessous a sa verification capturee
(regle post-incident: N artefacts nommes = N verifications; une
affirmation d'ABSENCE exige un grep de la source, pas une lecture).

1. "?boot.rq does state ..." -- grep de tools::Rd_db('quantreg')
   boot.rq.Rd, quantreg 6.1 (Packaged 2025-02-28, installee
   2026-04-24, donc la version au moment de notre premier email):
   phrase presente verbatim.
2. "?summary.rq ... does not mention it" -- grep length|na.action sur
   summary.rq.Rd: ZERO occurrence. Le bullet se="boot" renvoie a
   "See 'boot.rq' for further details".
3. "BIC(f) returns 2834.18" -- teste R 4.6.1: avec nobs.rq
   enregistree, BIC(f) = 2834.179 (stats:::BIC.default retombe sur
   nobs(object) quand logLik n'a pas l'attribut).
4. "as sandwich::vcovCL and clubSandwich::vcovCR do" -- re-teste
   empiriquement (lm avec 2 NA, cluster aux 2 longueurs):
   vcovCL TRUE, vcovCR(CR2) TRUE (matrices identiques).
5. "summary.rq already places the full dots ... only replaces it" --
   source verifiee: bargs <- modifyList(list(x,y,tau), dots) puis le
   bloc na.action remplace cluster.
6. Le patch lui-meme -- APPLIQUE et teste sur la repro engel:
   actuel+longueur ajustee = "cluster is wrong length";
   patche+ajustee = OK; patche+origine == actuel (meme graine);
   patche: origine == ajustee (meme graine).
7. "x and y at that point are the post-na.action data" -- source:
   x/y issus de model.frame(object) (233 lignes sur la repro).

## Reponse v3 ENVOYEE par Amal le 2026-07-28 (dans le fil)

Dear Professor Koenker,

Thank you for the reply, and for adding a nobs.rq method. It closes
both gaps of my first point: on R 4.6.1, stats::BIC() falls back to
nobs(object) when the logLik object carries no "nobs" attribute, so
BIC(f) on the engel example returns 2834.18 -- no change to
logLik.rq needed.

On the cluster point, you are right, and I owe you a correction: the
expected length is documented. ?boot.rq states it plainly: "the
length of the 'cluster' variable should always be the same as the
length of the original response variable before any 'na.action'
takes place." I had read only ?summary.rq -- whose se = "boot" item
points to boot.rq for details, a pointer I should have followed --
and wrongly wrote that neither page documents it. I apologize for
the inaccuracy.

My suggestion is therefore an optional convenience, not a fix.
sandwich::vcovCL and clubSandwich::vcovCR accept the cluster vector
at either the original or the fitted length, and code working
downstream from a fitted object naturally holds fitted-length
vectors; under the current unconditional subsetting such a vector is
subset by object$na.action a second time and reaches boot.rq at the
wrong length. Because summary.rq already passes the full dots
(cluster included) into bargs and the na.action block only replaces
that entry, guarding the replacement by length is enough:

    if (length(object$na.action) &&
        length(dots$cluster) ==
          length(object$residuals) + length(object$na.action)) {
        cluster <- dots$cluster[-object$na.action]
        bargs <- modifyList(bargs, list(cluster = cluster))
    }

I tested this guard on the engel reproduction from my first message:
the fitted-length call then runs, and under the same seed it gives
results identical to the original-length call, whose behaviour is
unchanged. The cases cannot be confused -- the lengths differ by
exactly length(object$na.action) > 0 -- and boot.rq's length check
still catches everything else.

If you prefer to keep the documented original-length convention
only, that is entirely reasonable; we have already adapted on our
side. A one-line pointer in ?summary.rq to the cluster paragraph of
?boot.rq would then spare future downstream authors my mistake.

With best regards, and thanks again,

Amal Tawfik
