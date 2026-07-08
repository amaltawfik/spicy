# console snapshot of the two-segment layout

    Code
      print(table_regression(fit))
    Output
      Discrete-choice multinomial logit (mlogit): mode
      
       Variable               │    B      SE       95% CI        p   
      ────────────────────────┼──────────────────────────────────────
       Alternative-invariant: │                                      
         price                │   -0.03  0.00  [-0.03, -0.02]  <.001 
         catch                │    0.36  0.11  [ 0.14,  0.57]   .001 
       boat:                  │                                      
         (Intercept)          │    0.53  0.22  [ 0.09,  0.96]   .018 
         income_k             │    0.09  0.05  [-0.01,  0.19]   .074 
       charter:               │                                      
         (Intercept)          │    1.69  0.22  [ 1.26,  2.13]  <.001 
         income_k             │   -0.03  0.05  [-0.13,  0.07]   .508 
       pier:                  │                                      
         (Intercept)          │    0.78  0.22  [ 0.35,  1.21]  <.001 
         income_k             │   -0.13  0.05  [-0.23, -0.03]   .012 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                      │ 1182                                 
       AIC                    │ 2446.3                               
      
      Note. Discrete-choice multinomial logit (mlogit).
      Std. errors: Wald asymptotic (z).
      Reference alternative: beach.

