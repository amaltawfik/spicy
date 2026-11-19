# the Pima GAM shows the age term it hid before

    Code
      print(table_regression(fit))
    Output
      Logistic GAM: type
      
       Variable            │   B      SE       95% CI        p   
      ─────────────────────┼─────────────────────────────────────
       (Intercept)         │  -8.13  0.84  [-9.77, -6.49]  <.001 
       bmi                 │   0.08  0.02  [ 0.04,  0.11]  <.001 
       glu                 │   0.04  0.00  [ 0.03,  0.04]  <.001 
       npreg               │   0.05  0.05  [-0.04,  0.14]   .244 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Smooth terms (edf): │                                     
         s(age)            │   3.63   –          –          .002 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                   │ 532                                 
       R² (Tjur)           │   0.36                              
       R² (McFadden)       │   0.32                              
       AIC                 │ 479.2                               
      
      Note. Logistic GAM. Std. errors: Bayesian (posterior covariance). Smooth terms: effective degrees of freedom (edf) and approximate test of each term. CI = confidence interval; AIC = Akaike information criterion.

---

    Code
      print(table_regression(fit, show_columns = c("b", "se", "t", "p"), stars = TRUE))
    Output
      Logistic GAM: type
      
       Variable            │     B       SE     z      p   
      ─────────────────────┼───────────────────────────────
       (Intercept)         │  -8.13***  0.84  -9.71  <.001 
       bmi                 │   0.08***  0.02   4.41  <.001 
       glu                 │   0.04***  0.00   8.51  <.001 
       npreg               │   0.05     0.05   1.16   .244 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Smooth terms (edf): │                               
         s(age)            │   3.63      –      –     .002 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                   │ 532                           
       R² (Tjur)           │   0.36                        
       R² (McFadden)       │   0.32                        
       AIC                 │ 479.2                         
      
      Note. Logistic GAM. Std. errors: Bayesian (posterior covariance). Smooth terms: effective degrees of freedom (edf) and approximate test of each term. AIC = Akaike information criterion.
      *** p < .001, ** p < .01, * p < .05.

