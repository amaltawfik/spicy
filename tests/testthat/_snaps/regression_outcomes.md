# console snapshot: a numeric exposure, one row per outcome

    Code
      print(table_regression_outcomes(d, outcomes = c(wellbeing_score, bmi, life_sat_health), exposure = income_k,
      covariates = c(age, sex)))
    Output
      Linear regression of each outcome on income_k
      
       Variable        │  N      B        95% CI        p   
      ─────────────────┼────────────────────────────────────
       wellbeing_score │ 1200   5.30  [ 4.73,  5.88]  <.001 
       bmi             │ 1188  -0.75  [-0.90, -0.60]  <.001 
       life_sat_health │ 1192   0.36  [ 0.31,  0.40]  <.001 
      
      Note. Linear regression models. One model per outcome, fitted separately. Adjusted for age and sex. Each model is fit on its own complete cases; N varies by outcome (1188–1200). Std. errors: classical (OLS). P-values adjusted across the 3 outcomes for 3 tests (Holm method). CI = confidence interval.

# console snapshot: a factor exposure, one block per outcome

    Code
      print(table_regression_outcomes(d, outcomes = c(wellbeing_score, bmi), exposure = employment_status,
      covariates = c(age, sex)))
    Output
      Linear regression of each outcome on employment_status
      
       Variable          │  N      B        95% CI        p   
      ───────────────────┼────────────────────────────────────
       wellbeing_score:  │ 1200                               
         Employed (ref.) │         –          –          –    
         Student         │        0.99  [-1.78,  3.75]  1.000 
         Unemployed      │       -3.82  [-6.37, -1.27]   .020 
         Inactive        │        0.14  [-2.83,  3.11]  1.000 
       bmi:              │ 1188                               
         Employed (ref.) │         –          –          –    
         Student         │       -0.07  [-0.74,  0.59]  1.000 
         Unemployed      │        0.14  [-0.48,  0.75]  1.000 
         Inactive        │        0.16  [-0.55,  0.88]  1.000 
      
      Note. Linear regression models. One model per outcome, fitted separately. Adjusted for age and sex. Each model is fit on its own complete cases; N varies by outcome (1188–1200). Std. errors: classical (OLS). P-values adjusted across the 2 outcomes for 6 tests (Holm method). CI = confidence interval.

# console snapshot: the intercepts shown

    Code
      print(table_regression_outcomes(d, outcomes = c(wellbeing_score, bmi), exposure = income_k,
      show_intercept = TRUE))
    Output
      Linear regression of each outcome on income_k
      
       Variable         │  N      B        95% CI        p   
      ──────────────────┼────────────────────────────────────
       wellbeing_score: │ 1200                               
         (Intercept)    │       48.58  [46.30, 50.85]  <.001 
         income_k       │        5.34  [ 4.78,  5.90]  <.001 
       bmi:             │ 1188                               
         (Intercept)    │       28.41  [27.81, 29.01]  <.001 
         income_k       │       -0.65  [-0.80, -0.50]  <.001 
      
      Note. Linear regression models. One model per outcome, fitted separately. Each model is fit on its own complete cases; N varies by outcome (1188–1200). Std. errors: classical (OLS). P-values adjusted across the 2 outcomes for 2 tests (Holm method). CI = confidence interval.

# console snapshot: scales mixed under exponentiate = TRUE

    Code
      print(table_regression_outcomes(d, outcomes = c(wellbeing_score, bmi, smoking), exposure = income_k,
      covariates = c(age, sex), family = list(smoking = stats::binomial()), exponentiate = TRUE))
    Output
      Regression of each outcome on income_k
      
       Variable            │  N    Estimate      95% CI        p   
      ─────────────────────┼───────────────────────────────────────
       wellbeing_score (B) │ 1200      5.30  [ 4.73,  5.88]  <.001 
       bmi (B)             │ 1188     -0.75  [-0.90, -0.60]  <.001 
       smoking (OR)        │ 1175      0.83  [ 0.75,  0.92]  <.001 
      
      Note. wellbeing_score, bmi: linear regression; smoking: logistic regression. One model per outcome, fitted separately. Adjusted for age and sex. smoking: coefficients exponentiated and displayed as OR; CI bounds exponentiated. Each model is fit on its own complete cases; N varies by outcome (1175–1200). Std. errors: wellbeing_score, bmi: classical (OLS); smoking: classical (Fisher information). P-values adjusted across the 3 outcomes for 3 tests (Holm method). CI = confidence interval.

