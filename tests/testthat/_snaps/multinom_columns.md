# columns-layout console render is stable

    Code
      print(table_regression(fit))
    Output
      Multinomial logistic regression: employment_status
      
                               Student             Unemployed           Inactive      
                         ────────────────────  ──────────────────  ────────────────── 
       Variable        │    B      SE     p      B     SE     p      B     SE     p   
      ─────────────────┼──────────────────────────────────────────────────────────────
       (Intercept)     │   -1.21  0.33  <.001  -1.37  0.31  <.001  -2.02  0.36  <.001 
       age             │   -0.01  0.01   .091  -0.00  0.01   .453   0.00  0.01   .731 
       sex:            │                                                              
         Female (ref.) │     –     –     –       –     –     –       –     –     –    
         Male          │    0.11  0.18   .545   0.22  0.17   .197   0.14  0.20   .465 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │ 1200                                                         
       AIC             │ 2540.1                                                       
      
      Note. Multinomial logistic regression.
      Std. errors: Wald asymptotic (z).
      Reference outcome: Employed.

