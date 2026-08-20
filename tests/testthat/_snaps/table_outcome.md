# the console shape is pinned

    Code
      print(suppressWarnings(table_outcome(d, bmi, by = c(sex, smoking))))
    Output
      Descriptive statistics of Body mass index
      
       Variable       │   M     SD    Min    Max   95% CI LL  95% CI UL   n     p   
      ────────────────┼─────────────────────────────────────────────────────────────
       Overall        │ 25.93  3.72  16.00  38.90    25.72      26.14    1188       
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Sex            │                                                        .018 
         Female       │ 25.69  3.78  16.00  38.90    25.39      25.98     616       
         Male         │ 26.20  3.64  16.00  37.70    25.90      26.50     572       
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Current smoker │                                                        .903 
         No           │ 25.96  3.76  16.00  38.90    25.72      26.21     915       
         Yes          │ 25.93  3.58  16.80  35.30    25.48      26.38     248       
         (Missing)    │ 24.74  3.63  17.60  32.50    23.24      26.23      25       
      
      Missing values removed: bmi (12). Group comparison: Welch t-test. Each block compares Body mass index across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.

---

    Code
      print(suppressWarnings(table_outcome(d, bmi, by = c(sex, region), statistic = TRUE,
      effect_size = "auto")))
    Output
      Descriptive statistics of Body mass index
      
       Variable            │   M     SD    Min    Max   95% CI LL  95% CI UL   n   
      ─────────────────────┼───────────────────────────────────────────────────────
       Overall             │ 25.93  3.72  16.00  38.90    25.72      26.14    1188 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Sex                 │                                                       
         Female            │ 25.69  3.78  16.00  38.90    25.39      25.98     616 
         Male              │ 26.20  3.64  16.00  37.70    25.90      26.50     572 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Region of residence │                                                       
         Central           │ 25.76  3.85  16.00  37.30    25.28      26.25     241 
         East              │ 25.96  3.98  16.00  37.70    25.30      26.61     144 
         North             │ 26.13  3.42  19.00  36.60    25.66      26.61     201 
         Other             │ 26.11  3.85  17.30  38.90    25.57      26.65     196 
         South             │ 25.71  3.58  16.00  35.30    25.21      26.21     198 
         West              │ 25.96  3.69  16.00  34.00    25.45      26.46     208 
      
       Variable            │         Test           p       ES     
      ─────────────────────┼───────────────────────────────────────
       Overall             │                                       
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Sex                 │   t(1184.50) = -2.38  .018  g = -0.14 
         Female            │                                       
         Male              │                                       
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Region of residence │ F(5, 531.84) = 0.47   .798  η² = 0.00 
         Central           │                                       
         East              │                                       
         North             │                                       
         Other             │                                       
         South             │                                       
         West              │                                       
      
      Missing values removed: bmi (12). Group comparison: Welch one-way ANOVA (region); Welch t-test (sex). Each block compares Body mass index across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.

---

    Code
      print(suppressWarnings(table_outcome(d, bmi, by = sex, overall = FALSE,
        show_columns = c("med_iqr", "n"))))
    Output
      Descriptive statistics of Body mass index
      
       Variable   │      Med [Q1, Q3]         n      p    
      ────────────┼───────────────────────────────────────
       Sex        │                                 .038  
         Female   │  25.70 [23.10, 28.60]    616          
         Male     │  26.10 [23.87, 28.63]    572          
      
      Missing values removed: bmi (12). Group comparison: Wilcoxon rank-sum test. Med [Q1, Q3] = median [first quartile, third quartile]. Each block compares Body mass index across the levels of one variable; blocks are not adjusted for one another.

