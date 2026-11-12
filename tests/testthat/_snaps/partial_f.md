# console snapshot of the partial F column

    Code
      print(table_regression(fit, show_columns = c("b", "se", "p", "partial_f")))
    Output
      Gamma regression: mpg
      
       Variable        │   B      SE     p          F       
      ─────────────────┼────────────────────────────────────
       (Intercept)     │   3.67  0.09  <.001                
       wt              │  -0.17  0.04  <.001  22.94 (1, 28) 
       cyl:            │                                    
         4 (ref.)      │    –     –     –                   
         6             │  -0.15  0.07   .028   6.09 (2, 28) 
         8             │  -0.27  0.08   .002   6.09 (2, 28) 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │  32                                
       R² (McFadden)   │   0.31                             
       R² (Nagelkerke) │   0.86                             
       AIC             │ 149.2                              
      
      Note. Gamma regression. Std. errors: classical (Fisher information). F = partial F statistic; AIC = Akaike information criterion.

