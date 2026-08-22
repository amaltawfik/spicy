# the console table prints its blocks, its spanners and its footer

    Code
      table_categorical_svy(d, select = c(stype, awards))
    Output
      Categorical table
      
       Variable   │   n      %    
      ────────────┼───────────────
       stype      │               
         E        │  144    78.7  
         H        │   14     7.7  
         M        │   25    13.7  
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       awards     │               
         No       │   53    29.0  
         Yes      │  130    71.0  
      
      N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Std. errors: Design-based (Taylor linearisation). Confidence intervals and tests use the design degrees of freedom. % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count.

---

    Code
      table_categorical_svy(d, select = stype, by = sch.wide)
    Output
      Categorical table by sch.wide
      
       Variable   │  No n    No %    Yes n    Yes %    Total n    Total %     p    
      ────────────┼────────────────────────────────────────────────────────────────
       stype      │                                                          .022  
         E        │   12     52.2     132     82.5       144       78.7            
         H        │    3     13.0      11      6.9        14        7.7            
         M        │    8     34.8      17     10.6        25       13.7            
      
      N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; degrees of freedom vary by group (9 to 14). Std. errors: Design-based (Taylor linearisation). Confidence intervals and tests use the design degrees of freedom. Group comparison: design-based Pearson chi-square (Rao-Scott second-order correction). % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count.

---

    Code
      table_categorical_svy(d, select = stype, proportion_ci = TRUE, deff = TRUE)
    Output
      Categorical table
      
       Variable   │   n      %      95% CI LL    95% CI UL    DEff  
      ────────────┼─────────────────────────────────────────────────
       stype      │                                                 
         E        │  144    78.7      67.1         87.0       2.40  
         H        │   14     7.7       3.5         15.8       1.91  
         M        │   25    13.7       8.4         21.3       1.40  
      
      N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Std. errors: Design-based (Taylor linearisation). Confidence intervals and tests use the design degrees of freedom. Percentage CIs: logit (survey::svyciprop). % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count. DEff = design effect (design-based variance / simple-random-sample variance at the same n).

