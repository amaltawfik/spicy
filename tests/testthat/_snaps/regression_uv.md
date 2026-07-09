# console snapshot: screen + multivariable merge

    Code
      print(table_regression_uv(d, outcome = smoking, predictors = c(age, bmi, sex), exponentiate = TRUE))
    Output
      Univariate and multivariable logistic regression: smoking
      
                                   Univariate                       Multivariable            
                         ───────────────────────────────  ────────────────────────────────── 
       Variable        │  N     OR      95% CI       p       N      OR      95% CI       p   
      ─────────────────┼─────────────────────────────────────────────────────────────────────
       age             │ 1175  1.01  [1.00, 1.01]   .292           1.00  [1.00, 1.01]   .344 
       bmi             │ 1163  1.00  [0.96, 1.04]   .905           1.00  [0.96, 1.03]   .822 
       sex:            │                                                                     
         Female (ref.) │    –   –         –         –         –     –         –         –    
         Male          │ 1175  0.95  [0.72, 1.26]   .713           0.95  [0.72, 1.26]   .726 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │                                  1163                               
       R² (McFadden)   │                                     0.00                            
       R² (Nagelkerke) │                                     0.00                            
       AIC             │                                  1212.3                             
      
      Note. Logistic regression models.
      Std. errors: classical (Fisher information).
      OR = odds ratio.
      Coefficients exponentiated and displayed as OR; CI bounds exponentiated.
      Each univariate model is fit on its own complete cases; N varies by predictor (1163-1175).

