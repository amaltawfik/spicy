# snapshot – single lm default rendering

    Code
      cat(capture_norm(out))
    Output
      Linear regression: mpg
      
       Variable    │   B     SE       95% CI        p
      ─────────────┼────────────────────────────────────
       (Intercept) │ 33.99  1.89  [30.12, 37.86]  <.001
       wt          │ -3.21  0.75  [-4.75, -1.66]  <.001
       cyl:        │
         4 (ref.)  │   –     –          –          –
         6         │ -4.26  1.39  [-7.09, -1.42]   .005
         8         │ -6.07  1.65  [-9.46, -2.69]  <.001
      ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n           │ 32
       R²          │  0.84
       Adj. R²     │  0.82
      
      Note. Linear regression. Std. errors: classical (OLS). CI = confidence interval.

# snapshot – multi-model with nested = TRUE comparison footer

    Code
      cat(capture_norm(out))
    Output
      Hierarchical linear regression: mpg
      
                          Model 1                 Model 2
                     ──────────────────  ──────────────────────────
       Variable    │   B     SE     p          B         SE     p
      ─────────────┼────────────────────────────────────────────────
       (Intercept) │ 37.29  1.88  <.001  33.99          1.89  <.001
       wt          │ -5.34  0.56  <.001  -3.21          0.75  <.001
       cyl:        │
         4 (ref.)  │                       –             –     –
         6         │                     -4.26          1.39   .005
         8         │                     -6.07          1.65  <.001
      ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n           │ 32                  32
       R²          │  0.75                0.84
       Adj. R²     │  0.74                0.82
       ΔR²         │   –                 +0.08
       F-change    │   –                  7.29 (2, 28)
       p (change)  │   –                   .003
      
      Note. Linear regression models. Std. errors: classical (OLS).

# snapshot – standardized + stars + reference annotation

    Code
      cat(capture_norm(out))
    Output
      Linear regression: mpg
      
       Variable      │    B        β     SE       95% CI        p
      ───────────────┼──────────────────────────────────────────────
       (Intercept)   │ 33.99***   0.60  1.89  [30.12, 37.86]  <.001
       wt            │ -3.21***  -0.52  0.75  [-4.75, -1.66]  <.001
       cyl: [ref: 4] │
         6           │ -4.26**   -0.71  1.39  [-7.09, -1.42]   .005
         8           │ -6.07***  -1.01  1.65  [-9.46, -2.69]  <.001
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n             │ 32
       R²            │  0.84
       Adj. R²       │  0.82
      
      Note. Linear regression. Std. errors: classical (OLS). β = standardized coefficient ("refit": outcome and numeric predictors z-scored, factor dummies on 0/1); CI = confidence interval.
      *** p < .001, ** p < .01, * p < .05.

# show_columns = "none" prints the fit statistics alone

    Code
      print(x)
    Output
      Hierarchical logistic regression: dentist_12m
      
       Variable   │ Model 1    Model 2      Model 3   
      ────────────┼───────────────────────────────────
       n          │ 1175     1175         1175        
       R² (Tjur)  │    0.00     0.00         0.05     
       AIC        │ 1417.9   1421.8       1370.6      
       BIC        │ 1428.0   1442.1       1401.0      
       Δχ²        │     –       0.09 (2)    55.18 (2) 
       p (change) │     –        .957        <.001    
      
      Note. Logistic regression models. AIC = Akaike information criterion; BIC = Bayesian information criterion.

---

    Code
      print(table_regression(m$m1, show_columns = "none"))
    Output
      Logistic regression: dentist_12m
      
       Variable        │ Model 1 
      ─────────────────┼─────────
       n               │ 1175    
       R² (McFadden)   │    0.00 
       R² (Nagelkerke) │    0.00 
       AIC             │ 1417.9  
      
      Note. Logistic regression. AIC = Akaike information criterion.

---

    Code
      print(table_regression(unname(m), show_columns = "none", nested = TRUE,
      show_fit_stats = .FO_GLM_STATS))
    Output
      Régression logistique — modèles emboîtés : dentist_12m
      
       Variable      │ Modèle 1   Modèle 2     Modèle 3   
      ───────────────┼────────────────────────────────────
       n             │  1175     1175         1175        
       R² (Tjur)     │     0,00     0,00         0,05     
       AIC           │  1417,9   1421,8       1370,6      
       BIC           │  1428,0   1442,1       1401,0      
       Δχ²           │      –       0,09 (2)    55,18 (2) 
       p (variation) │      –       0,957       <0,001    
      
      Note. Modèles de régression logistique. AIC = critère d'information d'Akaike ; BIC = critère d'information bayésien.

