# the console table prints its title, its columns and its footer

    Code
      table_continuous_svy(d, select = c(api00, api99))
    Output
      Descriptive statistics
      
       Variable │   M       SD     Min     Max    95% CI LL  95% CI UL   n  
      ──────────┼───────────────────────────────────────────────────────────
       api00    │ 644.17  105.75  411.00  905.00   593.68     694.66    183 
       api99    │ 606.98  112.85  365.00  890.00   555.02     658.94    183 
      
      Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Std. errors: Design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. CI = confidence interval; LL = lower limit; UL = upper limit.

---

    Code
      table_continuous_svy(d, select = api00, by = stype, statistic = TRUE)
    Output
      Descriptive statistics by stype
      
       Variable │ Group    M       SD     Min     Max    95% CI LL  95% CI UL   n  
      ──────────┼──────────────────────────────────────────────────────────────────
       api00    │ E      648.87  106.16  436.00  905.00   600.91     696.83    144 
                │ H      618.57   96.74  443.00  724.00   528.67     708.48     14 
                │ M      631.44  109.06  411.00  847.00   561.87     701.01     25 
      
       Variable │ Group       Test         p   
      ──────────┼──────────────────────────────
       api00    │ E      F(2, 12) = 1.28  .314 
                │ H                            
                │ M                            
      
      Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; degrees of freedom vary by group (7 to 14). Std. errors: Design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. Group comparison: design-based Wald test. The group comparison uses 12 degrees of freedom (observed groups only). CI = confidence interval; LL = lower limit; UL = upper limit.

---

    Code
      table_continuous_svy(d, select = api00, show_columns = c("m", "se", "med_iqr",
        "n", "weighted_n", "deff"), deff = TRUE)
    Output
      Descriptive statistics
      
       Variable │   M      SE         Mdn [Q1, Q3]         n   Weighted n  DEff 
      ──────────┼───────────────────────────────────────────────────────────────
       api00    │ 644.17  23.54  652.00 [552.00, 719.00]  183   6194.00    9.35 
      
      Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Quantiles: qrule = "math" (survey). Std. errors: Design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. SE = design-based standard error of the mean; Q1 = first quartile; Q3 = third quartile; DEff = design effect (design-based variance / simple-random-sample variance at the same n).

