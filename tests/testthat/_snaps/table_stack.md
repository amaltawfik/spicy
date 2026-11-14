# stacked lm, glm and coxph print as pinned

    Code
      print(table_regression(models, layout = "stacked"))
    Output
      Regression comparison
      
       Variable        │    B      SE       95% CI        p   
      ─────────────────┼──────────────────────────────────────
       BMI:            │                                      
       (Intercept)     │   23.95  0.39  [23.18, 24.71]  <.001 
       age             │    0.04  0.01  [ 0.02,  0.05]  <.001 
       sex:            │                                      
         Female (ref.) │     –     –          –          –    
         Male          │    0.52  0.21  [ 0.10,  0.94]   .015 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │ 1188                                 
       R²              │    0.02                              
       Adj. R²         │    0.02                              
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Dentist:        │                                      
       (Intercept)     │    0.44  0.23  [-0.00,  0.89]   .052 
       age             │    0.01  0.00  [ 0.00,  0.02]   .044 
       sex:            │                                      
         Female (ref.) │     –     –          –          –    
         Male          │    0.00  0.13  [-0.25,  0.25]   .978 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │ 1200                                 
       R² (Tjur)       │    0.00                              
       R² (McFadden)   │    0.00                              
       AIC             │ 1457.7                               
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Survival:       │                                      
       age             │    0.02  0.01  [-0.00,  0.04]   .065 
       sex             │   -0.51  0.17  [-0.84, -0.19]   .002 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │  228                                 
       N events        │  165                                 
       AIC             │ 1489.7                               
      
      Note. BMI: linear regression; Dentist: logistic regression; Survival: Cox proportional hazards regression. Survival: Concordance C = 0.60 (SE = 0.03). Std. errors: BMI: classical (OLS), Dentist: classical (Fisher information), Survival: Wald asymptotic (z). CI = confidence interval; AIC = Akaike information criterion.

# a stack with a header per panel prints as pinned

    Code
      print(table_regression(models, exponentiate = TRUE, layout = "stacked"))
    Output
      Regression comparison
      
       Variable        │    B      SE       95% CI        p   
      ─────────────────┼──────────────────────────────────────
       BMI:            │                                      
       (Intercept)     │   23.95  0.39  [23.18, 24.71]  <.001 
       age             │    0.04  0.01  [ 0.02,  0.05]  <.001 
       sex:            │                                      
         Female (ref.) │     –     –          –          –    
         Male          │    0.52  0.21  [ 0.10,  0.94]   .015 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │ 1188                                 
       R²              │    0.02                              
       Adj. R²         │    0.02                              
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Dentist:        │                                      
                       │   OR      SE       95% CI        p   
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       (Intercept)     │    1.56  0.36  [ 1.00,  2.44]   .052 
       age             │    1.01  0.00  [ 1.00,  1.02]   .044 
       sex:            │                                      
         Female (ref.) │     –     –          –          –    
         Male          │    1.00  0.13  [ 0.78,  1.29]   .978 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │ 1200                                 
       R² (Tjur)       │    0.00                              
       R² (McFadden)   │    0.00                              
       AIC             │ 1457.7                               
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Survival:       │                                      
                       │   HR      SE       95% CI        p   
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       age             │    1.02  0.01  [ 1.00,  1.04]   .065 
       sex             │    0.60  0.10  [ 0.43,  0.83]   .002 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │  228                                 
       N events        │  165                                 
       AIC             │ 1489.7                               
      
      Note. BMI: linear regression; Dentist: logistic regression; Survival: Cox proportional hazards regression. Dentist: coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric). Dentist: the (Intercept) row estimates the baseline odds. Survival: Concordance C = 0.60 (SE = 0.03). Survival: coefficients exponentiated and displayed as HR; SE on the HR scale (delta method); CI bounds exponentiated (asymmetric). Std. errors: BMI: classical (OLS), Dentist: classical (Fisher information), Survival: Wald asymptotic (z). CI = confidence interval; AIC = Akaike information criterion; HR = hazard ratio.

# a stacked hierarchy prints as pinned

    Code
      print(table_regression(.st_hierarchy(), nested = TRUE, layout = "stacked"))
    Output
      Hierarchical linear regression: mpg
      
       Variable              │       B         SE       95% CI        p   
      ───────────────────────┼────────────────────────────────────────────
       Model 1:              │                                            
       (Intercept)           │ 37.29          1.88  [33.45, 41.12]  <.001 
       wt                    │ -5.34          0.56  [-6.49, -4.20]  <.001 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                     │ 32                                         
       R²                    │  0.75                                      
       Adj. R²               │  0.74                                      
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Model 2:              │                                            
       (Intercept)           │ 37.23          1.60  [33.96, 40.50]  <.001 
       wt                    │ -3.88          0.63  [-5.17, -2.58]  <.001 
       hp                    │ -0.03          0.01  [-0.05, -0.01]   .001 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                     │ 32                                         
       R²                    │  0.83                                      
       Adj. R²               │  0.81                                      
       ΔR² vs Model 1        │ +0.07                                      
       F-change vs Model 1   │ 12.38 (1, 29)                              
       p (change) vs Model 1 │   .001                                     
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Model 3:              │                                            
       (Intercept)           │ 27.61          8.42  [10.36, 44.86]   .003 
       wt                    │ -4.36          0.75  [-5.90, -2.82]  <.001 
       hp                    │ -0.02          0.01  [-0.05,  0.01]   .244 
       qsec                  │  0.51          0.44  [-0.39,  1.41]   .255 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                     │ 32                                         
       R²                    │  0.83                                      
       Adj. R²               │  0.82                                      
       ΔR² vs Model 2        │ +0.01                                      
       F-change vs Model 2   │  1.35 (1, 28)                              
       p (change) vs Model 2 │   .255                                     
      
      Note. Linear regression models. Std. errors: classical (OLS). CI = confidence interval.

# a stack with column groups, split by width, prints as pinned

    Code
      print(table_regression(list(Health = .st_lm(), Employment = .st_mn()),
      layout = "stacked"))
    Output
      Regression comparison
      
       Variable        │    B      SE       95% CI        p   
      ─────────────────┼──────────────────────────────────────
       Health:         │                                      
       (Intercept)     │   23.95  0.39  [23.18, 24.71]  <.001 
       age             │    0.04  0.01  [ 0.02,  0.05]  <.001 
       sex:            │                                      
         Female (ref.) │     –     –          –          –    
         Male          │    0.52  0.21  [ 0.10,  0.94]   .015 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │ 1188                                 
       R²              │    0.02                              
       Adj. R²         │    0.02                              
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Employment:     │                                      
                       │            Student             Unem… 
                       │ ─────────────────────────────  ───── 
                       │    B      SE         p           B   
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       (Intercept)     │   -1.21  0.33           <.001  -1.37 
       age             │   -0.01  0.01            .091  -0.00 
       sex:            │                                      
         Female (ref.) │     –     –              –       –   
         Male          │    0.11  0.18            .545   0.22 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │ 1200                                 
       R² (McFadden)   │    0.00                              
       AIC             │ 2540.1                               
      
       Variable        │                                 
      ─────────────────┼─────────────────────────────────
       Employment:     │                                 
                       │ Unemployed        Inactive      
                       │ ───────────  ────────────────── 
                       │  SE     p      B     SE     p   
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       (Intercept)     │ 0.31  <.001  -2.02  0.36  <.001 
       age             │ 0.01   .453   0.00  0.01   .731 
       sex:            │                                 
         Female (ref.) │  –     –       –     –     –    
         Male          │ 0.17   .197   0.14  0.20   .465 
      
      Note. Health: linear regression; Employment: multinomial logistic regression. Employment: Reference outcome: Employed. Std. errors: Health: classical (OLS), Employment: Wald asymptotic (z). CI = confidence interval; AIC = Akaike information criterion.

