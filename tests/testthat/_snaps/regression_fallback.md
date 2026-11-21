# an unknown class is refused by default, with a line naming fallback

    Code
      table_regression(.fake_fit())
    Condition
      Error in `table_regression()`:
      ! Some `models` are not supported by `table_regression()`.
      Position 1: `spicy_fake_fit` – not a supported model class. If support would be useful, please open an issue: https://github.com/amaltawfik/spicy/issues
      i Supported model classes: lm, glm, negbin, rlm, nls, lm_robust, iv_robust, ivreg, tobit, rq, fixest, lmerMod, glmerMod, glmmTMB, lme, gls, polr, clm, clmm, multinom, mlogit, zeroinfl, hurdle, coxph, survreg, cph, flexsurvreg, svyglm, svyolr, svycoxph, geeglm, gam, betareg, selection, ols, lrm, Glm, stanreg, brmsfit. `table_regression_models()` lists them with their engines.
      i `fallback = "broom"` or `fallback = "parameters"` builds the rows from what `broom::tidy()` or `parameters::model_parameters()` returns.

# the rows are the tidier's, under its term names

    Code
      print(tb)
    Output
      Regression
      
       Variable    │   B     SE      95% CI      t     p   
      ─────────────┼───────────────────────────────────────
       (Intercept) │  1.00  0.20  [0.60, 1.40]  5.00  .001 
       x           │  0.50  0.10  [0.30, 0.70]  5.00  .002 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n           │ 20                                    
      
      Note. Regression. Estimates, standard errors, tests and intervals as returned by broom::tidy(). CI = confidence interval.

# the requests that need to know the model are refused

    Code
      table_regression(fit, fallback = "broom", exponentiate = TRUE)
    Condition
      Error in `table_regression()`:
      ! `exponentiate = TRUE` is not available for this model class (`spicy_fake_fit`).
      i Its rows come from `broom::tidy()`, and spicy does not know how the model was fitted.

# arima read by broom and by parameters: the numbers are the tidier's

    Code
      print(tb)
    Output
      Regression
      
       Variable  │   B     SE      95% CI    
      ───────────┼───────────────────────────
       ar1       │  0.57  0.12  [0.35, 0.80] 
       intercept │  2.41  0.15  [2.13, 2.70] 
      ╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n         │ 48                        
       AIC       │ 64.8                      
      
      Note. Regression. Estimates, standard errors, tests and intervals as returned by broom::tidy(). CI = confidence interval; AIC = Akaike information criterion.
    Code
      print(tp)
    Output
      Regression
      
       Variable  │   B     SE      90% CI       p   
      ───────────┼──────────────────────────────────
       ar1       │  0.57  0.12  [0.38, 0.76]  <.001 
       intercept │  2.41  0.15  [2.17, 2.65]  <.001 
      ╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n         │ 48                               
       AIC       │ 64.8                             
      
      Note. Regression. Estimates, standard errors, tests and intervals as returned by parameters::model_parameters(). CI = confidence interval; AIC = Akaike information criterion.

# beside an lm: one note per model, in English and in French

    Code
      print(tb)
    Output
      Regression comparison
      
                            OLS                AR       
                     ──────────────────  ────────────── 
       Variable    │   B     SE     p      B     SE   p 
      ─────────────┼────────────────────────────────────
       Outcome     │        mpg                         
       (Intercept) │ 37.29  1.88  <.001                 
       wt          │ -5.34  0.56  <.001                 
       ar1         │                      0.57  0.12  – 
       intercept   │                      2.41  0.15  – 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n           │ 32                  48             
       R²          │  0.75                 –            
       Adj. R²     │  0.74                 –            
      
      Note. OLS: linear regression; AR: regression. Std. errors: OLS: classical (OLS). AR: Estimates, standard errors, tests and intervals as returned by broom::tidy().
    Code
      print(tf)
    Output
      Régression — comparaison de modèles
      
                                     OLS                AR       
                             ───────────────────  ────────────── 
       Variable            │   B     ES     p       B     ES   p 
      ─────────────────────┼─────────────────────────────────────
       Variable dépendante │         mpg                         
       (Intercept)         │ 37,29  1,88  <0,001                 
       wt                  │ -5,34  0,56  <0,001                 
       ar1                 │                       0,57  0,12  – 
       intercept           │                       2,41  0,15  – 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                   │ 32                   48             
       R²                  │  0,75                  –            
       R² ajusté           │  0,74                  –            
      
      Note. OLS : régression linéaire ; AR : régression. Erreurs standards : OLS : classiques (MCO). AR : Estimations, erreurs standards, tests et intervalles tels que rendus par broom::tidy().

