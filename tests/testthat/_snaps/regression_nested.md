# snapshot - Wald change rows, lm HC3 and glm HC0, English and French

    Code
      cat(capture_norm_nested(table_regression(lm_fits, nested = TRUE, vcov = "HC3")))
    Output
      Hierarchical linear regression: y
      
                             Model 1              Model 2
                       ───────────────────  ────────────────────
       Variable      │   B      SE     p       B      SE     p
      ───────────────┼───────────────────────────────────────────
       (Intercept)   │   1.03  0.15  <.001    1.04   0.14  <.001
       x1            │   0.78  0.21  <.001    0.82   0.21  <.001
       x2            │                        0.44   0.15   .004
       x3            │                        0.19   0.12   .126
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n             │ 200                  200
       R²            │   0.13                 0.18
       Adj. R²       │   0.13                 0.17
       ΔR²           │    –                  +0.05
       Wald F-change │    –                  +5.39
       p (change)    │    –                    .005
      
      Note. Linear regression models.
      Std. errors: heteroskedasticity-robust (HC3).

---

    Code
      cat(capture_norm_nested(table_regression(glm_fits, nested = TRUE, vcov = "HC0")))
    Output
      Hierarchical logistic regression: yb
      
                               Model 1              Model 2
                          ──────────────────  ───────────────────
       Variable         │   B      SE    p       B      SE    p
      ──────────────────┼─────────────────────────────────────────
       (Intercept)      │  -0.05  0.14  .753   -0.04   0.15  .773
       x1               │   0.34  0.15  .019    0.38   0.15  .011
       x2               │                       0.33   0.15  .026
       x3               │                       0.08   0.15  .579
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                │ 200                 200
       R² (McFadden)    │   0.02                0.04
       R² (Nagelkerke)  │   0.04                0.07
       AIC              │ 275.2               273.6
       Wald χ² (change) │    –                 +5.31
       p (change)       │    –                   .070
      
      Note. Logistic regression models.
      Std. errors: heteroskedasticity-robust (HC0).

---

    Code
      cat(capture_norm_nested(table_regression(lm_fits, nested = TRUE, vcov = "HC3")))
    Output
      Régression linéaire — modèles hiérarchiques : y
      
                                     Model 1                Model 2
                               ────────────────────  ─────────────────────
       Variable              │   B      SE     p        B      SE     p
      ───────────────────────┼─────────────────────────────────────────────
       (Intercept)           │   1,03  0,15  <0,001    1,04   0,14  <0,001
       x1                    │   0,78  0,21  <0,001    0,82   0,21  <0,001
       x2                    │                         0,44   0,15   0,004
       x3                    │                         0,19   0,12   0,126
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                     │ 200                   200
       R²                    │   0,13                  0,18
       Adj. R²               │   0,13                  0,17
       ΔR²                   │    –                   +0,05
       F de Wald (variation) │    –                   +5,39
       p (variation)         │    –                    0,005
      
      Note. Modèles de régression linéaire.
      Erreurs types : robustes à l'hétéroscédasticité (HC3).

---

    Code
      cat(capture_norm_nested(table_regression(glm_fits, nested = TRUE, vcov = "HC0")))
    Output
      Régression logistique — modèles hiérarchiques : yb
      
                                      Model 1              Model 2
                                ───────────────────  ────────────────────
       Variable               │   B      SE     p       B      SE     p
      ────────────────────────┼───────────────────────────────────────────
       (Intercept)            │  -0,05  0,14  0,753   -0,04   0,15  0,773
       x1                     │   0,34  0,15  0,019    0,38   0,15  0,011
       x2                     │                        0,33   0,15  0,026
       x3                     │                        0,08   0,15  0,579
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                      │ 200                  200
       R² (McFadden)          │   0,02                 0,04
       R² (Nagelkerke)        │   0,04                 0,07
       AIC                    │ 275,2                273,6
       χ² de Wald (variation) │    –                  +5,31
       p (variation)          │    –                   0,070
      
      Note. Modèles de régression logistique.
      Erreurs types : robustes à l'hétéroscédasticité (HC0).

