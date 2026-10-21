# survey 4.5 : calibrate(), trimWeights() et svytotal() sur plans à poids répliqués

- **Cible** : survey 4.5 (sources R-Forge r406, identiques sur ces fonctions)
- **Canal** : courriel au mainteneur, faute de tracker public
- **Envoyé** : 2026-10-21
- **Statut** : envoyé
- **Côté spicy** : non exposé. spicy n'appelle ni `calibrate()` ni `trimWeights()` ; les plans sont construits par l'utilisateur. Rien à retirer quand survey corrigera.

---

*Dossier tel qu'envoyé, conservé verbatim ci-dessous.*
To: t.lumley@auckland.ac.nz
Subject: survey 4.5: three bugs with replicate-weight designs

Dear Professor Lumley,

I found three bugs with replicate-weight designs in survey 4.5. They are also in the R-Forge copy (revision 406). The first two come with a fix, tested as shown.

1. calibrate() fails when the replicate weights are not compressed

    library(survey)
    data(scd)
    repweights <- 2 * cbind(c(1,0,1,0,1,0), c(1,0,0,1,0,1),
                            c(0,1,1,0,0,1), c(0,1,0,1,1,0))
    scdrep <- svrepdesign(data = scd, type = "BRR", repweights = repweights,
                          combined.weights = FALSE)
    calibrate(scdrep, ~1, c(`(Intercept)` = 12))
    ## Error in if (compress || (is.na(compress && inherits(design$repweights,  :
    ##   missing value where TRUE/FALSE needed

With the default compress = NA and uncompressed replicate weights, the condition is NA. It appears in regcalibrate.svyrep.design (R/greg.R:259-260) and in calibrate.svyrep.design (R/grake.R:301-302). Moving the parenthesis alone is not enough, since NA || FALSE is still NA. Fix, in both places:

    -  if (compress ||
    -      (is.na(compress && inherits(design$repweights,"repweights_compressed")))){
    +  if (isTRUE(compress) ||
    +      (is.na(compress) && inherits(design$repweights,"repweights_compressed"))){

With it, the example calibrates: the sampling weights and every replicate column sum to 12. On apistrat, with the replicate weights of as.svrepdesign(dstrat) passed to svrepdesign() as a data frame, the result equals calibrating the compressed design.

2. trimWeights() does not trim the sampling weights of a replicate design

    data(api)
    dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
    svytotal(~api00, trimWeights(dstrat, upper = 40))   # 4084012 (SE 56125)
    rstrat <- as.svrepdesign(dstrat)
    svytotal(~api00, rstrat)                            # 4102208 (SE 59067)
    svytotal(~api00, trimWeights(rstrat, upper = 40))   # 4102208 (SE 49688)

trimWeights.svyrep.design stores the trimmed weights in design$prob (R/grake.R:494), which replicate designs do not read: weights.svyrep.design returns object$pweights. The replicate weights are trimmed, so the estimate is untrimmed while its standard error comes from trimmed replicates. Fix:

    -      design$prob<-1/pw
    +      design$pweights<-pw

With it, the estimate is 4084012, as for the svydesign() object, and the sampling weights equal weights(trimWeights(dstrat, upper = 40)).

3. svytotal() fails after trimWeights() on a replicate design with an fpc

    dstrat_f <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
                          data = apistrat, fpc = ~fpc)
    rstrat_f <- trimWeights(as.svrepdesign(dstrat_f), upper = 40)
    svytotal(~api00, rstrat_f)
    ## Error in svrVar(repmeans, scale, rscales, mse = design$mse, coef = rval) :
    ##   All replicates contained NAs

svymean() works on the same design. In svytotal.svyrep.design, pw is set to 1 when combined.weights is TRUE (R/surveyrep.R:1008-1011), then subset with pw[!design$selfrep] (lines 1031 and 1042), which gives 1 followed by NAs. I have not prepared a fix for this one.

I sent two other reports on 21 August; I do not know whether they reached you.

Best regards,
Amal Tawfik
