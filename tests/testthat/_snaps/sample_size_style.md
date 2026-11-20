# spicy_style() takes sample_size and refuses any other value

    Code
      spicy_style(sample_size = "top")
    Condition
      Error in `spicy_style()`:
      ! `sample_size` must be one of "note", "header", "none".

# table_categorical(): the three places in the console

    Code
      print(table_categorical(d, c(smoking), by = sex, style = spicy_style(
        sample_size = v)))
    Output
      Categorical table by sex
      
                      Female      Male     (Missing)    Total              
                     ─────────  ─────────  ─────────  ─────────            
       Variable    │  n    %     n    %    n     %     n    %     p     φ  
      ─────────────┼───────────────────────────────────────────────────────
       smoking     │                                             .725  .01 
         No        │ 474  78.3  449  79.2  3   100.0  926  78.8            
         Yes       │ 131  21.7  118  20.8  0     0.0  249  21.2            
         (Missing) │  14         11        0           25                  
      
      Note. N = 1200: Female (n = 619), Male (n = 578), (Missing) (n = 3). Percentages are column percentages. P-value: Pearson chi-squared test.

---

    Code
      print(table_categorical(d, c(smoking), by = sex, style = spicy_style(
        sample_size = v)))
    Output
      Categorical table by sex
      
                     Female (n = 619)  Male (n = 578)  (Missing) (n = 3) 
                     ────────────────  ──────────────  ───────────────── 
       Variable    │    n        %       n       %       n         %     
      ─────────────┼─────────────────────────────────────────────────────
       smoking     │                                                     
         No        │   474     78.3     449     79.2     3       100.0   
         Yes       │   131     21.7     118     20.8     0         0.0   
         (Missing) │    14               11              0               
      
                     Total (n = 1200)            
                     ────────────────            
       Variable    │    n        %      p     φ  
      ─────────────┼─────────────────────────────
       smoking     │                   .725  .01 
         No        │   926     78.8              
         Yes       │   249     21.2              
         (Missing) │    25                       
      
      Note. N = 1200. Percentages are column percentages. P-value: Pearson chi-squared test.

---

    Code
      print(table_categorical(d, c(smoking), by = sex, style = spicy_style(
        sample_size = v)))
    Output
      Categorical table by sex
      
                      Female      Male     (Missing)    Total              
                     ─────────  ─────────  ─────────  ─────────            
       Variable    │  n    %     n    %    n     %     n    %     p     φ  
      ─────────────┼───────────────────────────────────────────────────────
       smoking     │                                             .725  .01 
         No        │ 474  78.3  449  79.2  3   100.0  926  78.8            
         Yes       │ 131  21.7  118  20.8  0     0.0  249  21.2            
         (Missing) │  14         11        0           25                  
      
      Note. Percentages are column percentages. P-value: Pearson chi-squared test.

# table_continuous_lm(): the three places, and the engines

    Code
      print(table_continuous_lm(d, c(bmi), by = sex, style = spicy_style(sample_size = v)))
    Output
      Continuous outcomes by sex
      
                            M                      95% CI                             
                    ─────────────────          ──────────────                         
       Variable   │  Female    Male      Δ       LL      UL      p       R²      n    
      ────────────┼───────────────────────────────────────────────────────────────────
       bmi        │  25.69     26.21    0.52    0.09    0.94    .017    0.00    1185  
      
      Note. N = 1197: Female (n = 619), Male (n = 578). Missing values removed: bmi (12), sex (3). P-value: t-test from the linear model. Δ = Male − Female. CI = confidence interval; LL = lower limit; UL = upper limit.

---

    Code
      print(table_continuous_lm(d, c(bmi), by = sex, style = spicy_style(sample_size = v)))
    Output
      Continuous outcomes by sex
      
                                 M                          95% CI               
                  ────────────────────────────────        ──────────             
       Variable │ Female (n = 619)  Male (n = 578)   Δ     LL    UL    p     R²  
      ──────────┼────────────────────────────────────────────────────────────────
       bmi      │      25.69            26.21       0.52  0.09  0.94  .017  0.00 
      
       Variable │  n   
      ──────────┼──────
       bmi      │ 1185 
      
      Note. N = 1197. Missing values removed: bmi (12), sex (3). P-value: t-test from the linear model. Δ = Male − Female. CI = confidence interval; LL = lower limit; UL = upper limit.

---

    Code
      print(table_continuous_lm(d, c(bmi), by = sex, style = spicy_style(sample_size = v)))
    Output
      Continuous outcomes by sex
      
                            M                      95% CI                             
                    ─────────────────          ──────────────                         
       Variable   │  Female    Male      Δ       LL      UL      p       R²      n    
      ────────────┼───────────────────────────────────────────────────────────────────
       bmi        │  25.69     26.21    0.52    0.09    0.94    .017    0.00    1185  
      
      Note. Missing values removed: bmi (12), sex (3). P-value: t-test from the linear model. Δ = Male − Female. CI = confidence interval; LL = lower limit; UL = upper limit.

# table_categorical_svy(): the three places, and the engines

    Code
      print(table_categorical_svy(des, c(sch.wide), by = stype, drop_na = FALSE,
      style = spicy_style(sample_size = v)))
    Output
      Categorical table by stype
      
                      E         H         M      (Missing)    Total         
                  ─────────  ────────  ────────  ─────────  ─────────       
       Variable │  n    %    n    %    n    %    n     %     n    %     p   
      ──────────┼───────────────────────────────────────────────────────────
       sch.wide │                                                      .022 
         No     │  12   8.4   3  23.1   8  32.0  0     0.0   23  12.6       
         Yes    │ 131  91.6  10  76.9  17  68.0  2   100.0  160  87.4       
      
      Note. N = 183 (weighted 6194). Percentages are column percentages. Design: cluster (dnum), 15 PSUs, with finite population correction; degrees of freedom vary by group (0–14). Std. errors: design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. P-value: design-based Pearson chi-squared (Rao-Scott second-order correction). % = estimated percentage. n = observed (unweighted) count.

---

    Code
      print(table_categorical_svy(des, c(sch.wide), by = stype, drop_na = FALSE,
      style = spicy_style(sample_size = v)))
    Output
      Categorical table by stype
      
                  E (n = 143)  H (n = 13)  M (n = 25)  (Missing) (n = 2) 
                  ───────────  ──────────  ──────────  ───────────────── 
       Variable │  n      %     n     %     n     %      n         %     
      ──────────┼────────────────────────────────────────────────────────
       sch.wide │                                                        
         No     │  12    8.4    3   23.1    8   32.0     0         0.0   
         Yes    │ 131   91.6   10   76.9   17   68.0     2       100.0   
      
                  Total (n = 183)       
                  ───────────────       
       Variable │   n        %      p   
      ──────────┼───────────────────────
       sch.wide │                  .022 
         No     │   23     12.6         
         Yes    │  160     87.4         
      
      Note. N = 183 (weighted 6194). Percentages are column percentages. Design: cluster (dnum), 15 PSUs, with finite population correction; degrees of freedom vary by group (0–14). Std. errors: design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. P-value: design-based Pearson chi-squared (Rao-Scott second-order correction). % = estimated percentage. n = observed (unweighted) count.

---

    Code
      print(table_categorical_svy(des, c(sch.wide), by = stype, drop_na = FALSE,
      style = spicy_style(sample_size = v)))
    Output
      Categorical table by stype
      
                      E         H         M      (Missing)    Total         
                  ─────────  ────────  ────────  ─────────  ─────────       
       Variable │  n    %    n    %    n    %    n     %     n    %     p   
      ──────────┼───────────────────────────────────────────────────────────
       sch.wide │                                                      .022 
         No     │  12   8.4   3  23.1   8  32.0  0     0.0   23  12.6       
         Yes    │ 131  91.6  10  76.9  17  68.0  2   100.0  160  87.4       
      
      Note. Percentages are column percentages. Design: cluster (dnum), 15 PSUs, with finite population correction; degrees of freedom vary by group (0–14). Std. errors: design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. P-value: design-based Pearson chi-squared (Rao-Scott second-order correction). % = estimated percentage. n = observed (unweighted) count.

# jama and lancet: the console tables under the theme

    Code
      print(table_categorical(d, c(smoking), by = sex, style = th))
    Output
      Categorical table by sex
      
                     Female (n = 619)  Male (n = 578)  (Missing) (n = 3) 
                     ────────────────  ──────────────  ───────────────── 
       Variable    │    n        %       n       %       n         %     
      ─────────────┼─────────────────────────────────────────────────────
       smoking     │                                                     
         No        │   474     78.3     449     79.2     3       100.0   
         Yes       │   131     21.7     118     20.8     0         0.0   
         (Missing) │    14               11              0               
      
                     Total (n = 1200)           
                     ────────────────           
       Variable    │    n        %      p    φ  
      ─────────────┼────────────────────────────
       smoking     │                   .72  .01 
         No        │   926     78.8             
         Yes       │   249     21.2             
         (Missing) │    25                      
      
      Abbreviation: φ, phi coefficient.
      N = 1200. Percentages are column percentages. P-value: Pearson chi-squared test.

---

    Code
      print(table_continuous_lm(d, c(bmi), by = sex, style = th))
    Output
      Continuous outcomes by sex
      
                                Mean                        95% CI                    
                  ────────────────────────────────        ──────────                  
       Variable │ Female (n = 619)  Male (n = 578)   Δ     LL    UL    p    R²    n   
      ──────────┼─────────────────────────────────────────────────────────────────────
       bmi      │      25.69            26.21       0.52  0.09  0.94  .02  0.00  1185 
      
      Abbreviations: LL, lower limit; UL, upper limit.
      N = 1197. Missing values removed: bmi (12), sex (3). P-value: t-test from the linear model. Δ = Male − Female.

---

    Code
      print(table_categorical(d, c(smoking), by = sex, style = th))
    Output
      Categorical table by sex
      
                     Female (n = 619)  Male (n = 578)  (Missing) (n = 3) 
                     ────────────────  ──────────────  ───────────────── 
       Variable    │    n        %       n       %       n         %     
      ─────────────┼─────────────────────────────────────────────────────
       smoking     │                                                     
         No        │   474     78·3     449     79·2     3       100·0   
         Yes       │   131     21·7     118     20·8     0         0·0   
         (Missing) │    14               11              0               
      
                     Total (n = 1200)             
                     ────────────────             
       Variable    │    n        %      p     φ   
      ─────────────┼──────────────────────────────
       smoking     │                   0·72  0·01 
         No        │   926     78·8               
         Yes       │   249     21·2               
         (Missing) │    25                        
      
      N = 1200. Percentages are column percentages. P-value: Pearson chi-squared test. φ=phi coefficient.

---

    Code
      print(table_continuous_lm(d, c(bmi), by = sex, style = th))
    Output
      Continuous outcomes by sex
      
                                Mean                        95% CI                
                  ────────────────────────────────        ──────────              
       Variable │ Female (n = 619)  Male (n = 578)   Δ     LL    UL     p     R²  
      ──────────┼─────────────────────────────────────────────────────────────────
       bmi      │      25·69            26·21       0·52  0·09  0·94  0·017  0·00 
      
       Variable │  n   
      ──────────┼──────
       bmi      │ 1185 
      
      N = 1197. Missing values removed: bmi (12), sex (3). P-value: t-test from the linear model. Δ = Male − Female. LL=lower limit. R²=coefficient of determination. UL=upper limit.

