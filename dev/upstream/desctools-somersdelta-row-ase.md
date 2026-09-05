# DescTools : SomersDelta(direction = "row"), ASE sur un vecteur de marges mal aligné

- **Cible** : DescTools 0.99.60
- **Canal** : GitHub, AndriSignorell/DescTools#184, https://github.com/AndriSignorell/DescTools/issues/184
- **Envoyé** : 2026-07-23
- **Statut** : ouvert au 2026-09-06
- **Côté spicy** : oracle interne survival::concordance ; correctif byrow proposé upstream

---

*Dossier tel qu'envoyé, conservé verbatim ci-dessous.*

# POSTEE 2026-07-25: https://github.com/AndriSignorell/DescTools/issues/184
# Dedup fait 2026-07-25 (2 recherches, aucun doublon). Verifie sur
# DescTools 0.99.60. Oracles: survival::concordance (7 decimales sur
# les 3 tables, les deux directions) + delta-methode numerique; le fix
# byrow reproduit ces valeurs a 7 decimales partout. PSPP retire du
# texte a la demande d'Amal (concordance = oracle R natif superieur).
# Sonde v1 (confusion de direction) corrigee avant redaction.
# A poster sous le compte d'Amal apres son go.

**Title:** SomersDelta(direction = "row"): ASE uses a misaligned margin vector (recycled along rows instead of columns)

**Body:**

In `SomersDelta()`, the asymptotic variance `sigma2` combines cell-level matrices with a margin-based vector:

```r
## excerpt from the body of DescTools::SomersDelta (0.99.60);
## x holds the cell-level concordance/discordance quantities
n <- sum(tab)
switch(match.arg(arg = direction, choices = c("row", "column")),
  row = {
    ni. <- colSums(tab)
  }, column = {
    ni. <- rowSums(tab)
  })
wt <- n^2 - sum(ni.^2)
sigma2 <- 4/wt^4 * (sum(tab * (wt * (x$pi.c - x$pi.d) - 2 * (x$C - x$D) * (n - ni.))^2))
```

Inside the parentheses, the length-`ncol`/`nrow` vector `(n - ni.)` is subtracted from the matrix `wt * (x$pi.c - x$pi.d)`, and R recycles it along the FIRST dimension (down the rows). For `direction = "column"` (`ni. <- rowSums(tab)`, length `nrow`) that alignment happens to be correct. For `direction = "row"` (`ni. <- colSums(tab)`, length `ncol`) the column margins land on row positions, so every cell picks the wrong margin whenever the table is not symmetric. Re-implementing the formula with the recycled vector reproduces `SomersDelta`'s value exactly, which confirms the mechanism.

The point estimate is unaffected; only the ASE (hence the CI) is wrong for `direction = "row"`, by an amount that grows with margin asymmetry.

Reproducible example (DescTools 0.99.60, the current CRAN release):

```r
tab <- as.table(matrix(c(30, 2, 20, 4, 8, 15, 1, 40), nrow = 4,
  dimnames = list(r = paste0("r", 1:4), c = c("c1", "c2"))))

out <- DescTools::SomersDelta(tab, direction = "row", conf.level = 0.95)
unname((out["upr.ci"] - out["lwr.ci"]) / (2 * qnorm(0.975)))
#> 0.0831744   <-- implied ASE

## survival::concordance() computes the same Somers' d with its SE:
df <- as.data.frame(tab)
rv <- rep(as.numeric(df$r), df$Freq)
cv <- rep(as.numeric(df$c), df$Freq)
fit <- survival::concordance(cv ~ rv)
c(d = 2 * fit$concordance - 1, se = 2 * sqrt(fit$var))
#>          d         se
#> 0.55524554 0.08212864
```

The estimates agree exactly (`0.5552455` both), the SEs do not (`0.0831744` vs `0.0821286`). A numeric delta-method check (gradient of d over the multinomial cell probabilities, `numDeriv::grad` + multinomial covariance) gives `0.0821286` as well, to 7 decimals. `direction = "column"` matches both references to 7 decimals on every table we tried (as expected: there the recycled vector has length `nrow` and aligns correctly); `direction = "row"` is off by 0.04% to 1.3% on our test tables (a 2x4, a 3x5, and the 4x2 above), growing with asymmetry.

Suggested fix - orient the margin term explicitly instead of relying on recycling:

```r
adj <- switch(direction,
  row    = matrix(n - ni., nrow = nrow(tab), ncol = ncol(tab), byrow = TRUE),
  column = matrix(n - ni., nrow = nrow(tab), ncol = ncol(tab))
)
sigma2 <- 4/wt^4 * sum(tab * (wt * (x$pi.c - x$pi.d) - 2 * (x$C - x$D) * adj)^2)
```

With this change the `survival::concordance()` and delta-method values are reproduced to 7 decimals on every table we tested, for both directions.

Found while cross-validating a downstream package's association measures; happy to provide more tables.
