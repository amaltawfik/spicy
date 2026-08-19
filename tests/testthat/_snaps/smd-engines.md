# the default rendering is frozen under snapshot

    Code
      table_continuous(d, select = c(x, y), by = g)
    Output
      Descriptive statistics by g
      
       Variable │ Group   M     SD   Min   Max   95% CI LL  95% CI UL  n   p   
      ──────────┼──────────────────────────────────────────────────────────────
       x        │ A      3.00  1.83  1.00  5.00     0.09       5.91    4  .565 
                │ B      4.33  3.21  2.00  8.00    -3.65      12.32    3       
      ╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       y        │ A      2.25  1.50  1.00  4.00    -0.14       4.64    4  .264 
                │ B      5.33  3.51  2.00  9.00    -3.39      14.06    3       

---

    Code
      table_categorical(d, select = c(bin, k3), by = g)
    Output
      Categorical table by g
      
       Variable │ A n  A %   B n  B %   Total n  Total %   p    Effect size 
      ──────────┼───────────────────────────────────────────────────────────
       bin      │                                         .270      .42     
         no     │  3   75.0   1   33.3     4      57.1                      
         yes    │  1   25.0   2   66.7     3      42.9                      
      ╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       k3       │                                         .459      .47     
         a      │  2   50.0   1   33.3     3      42.9                      
         b      │  1   25.0   2   66.7     3      42.9                      
         c      │  1   25.0   0    0.0     1      14.3                      
      
      Note. Phi: bin; Cramer's V: k3.

