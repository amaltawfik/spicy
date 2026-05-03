# print.spicy_freq_table: numeric vector with NAs

    Code
      freq(c(1, 2, 2, 3, 3, 3, NA))
    Output
      Frequency table: c(1, 2, 2, 3, 3, 3, NA)
      
       Category   │ Values      Freq.    Percent    Valid Percent 
      ────────────┼───────────────────────────────────────────────
       Valid      │ 1               1       14.3             16.7 
                  │ 2               2       28.6             33.3 
                  │ 3               3       42.9             50.0 
       Missing    │ NA              1       14.3                  
      ────────────┼───────────────────────────────────────────────
       Total      │                 7      100.0            100.0 
      
      Class: numeric
      Data: c(1, 2, 2, 3, 3, 3, NA)

# print.spicy_freq_table: cumulative + valid percent

    Code
      freq(c("a", "b", "b", "c", NA), cum = TRUE)
    Output
      Frequency table: c("a", "b", "b", "c", NA)
      
       Category   │ Values      Freq.    Percent    Valid Percent    Cum. Percent 
      ────────────┼───────────────────────────────────────────────────────────────
       Valid      │ a               1       20.0             25.0            20.0 
                  │ b               2       40.0             50.0            60.0 
                  │ c               1       20.0             25.0            80.0 
       Missing    │ NA              1       20.0                            100.0 
      ────────────┼───────────────────────────────────────────────────────────────
       Total      │                 5      100.0            100.0           100.0 
      
       Category   │ Values      Cum. Valid Percent 
      ────────────┼────────────────────────────────
       Valid      │ a                         25.0 
                  │ b                         75.0 
                  │ c                        100.0 
       Missing    │ NA                             
      ────────────┼────────────────────────────────
       Total      │                          100.0 
      
      Class: character
      Data: c("a", "b", "b", "c", NA)

# print.spicy_freq_table: French decimal mark

    Code
      freq(c(1, 2, 2, 3, 3, 3, NA), decimal_mark = ",")
    Output
      Frequency table: c(1, 2, 2, 3, 3, 3, NA)
      
       Category   │ Values      Freq.    Percent    Valid Percent 
      ────────────┼───────────────────────────────────────────────
       Valid      │ 1               1       14,3             16,7 
                  │ 2               2       28,6             33,3 
                  │ 3               3       42,9             50,0 
       Missing    │ NA              1       14,3                  
      ────────────┼───────────────────────────────────────────────
       Total      │                 7      100,0            100,0 
      
      Class: numeric
      Data: c(1, 2, 2, 3, 3, 3, NA)

# print.spicy_freq_table: weighted with rescale

    Code
      freq(df, g, weights = w, rescale = TRUE)
    Output
      Frequency table: g
      
       Category   │ Values      Freq.    Percent 
      ────────────┼──────────────────────────────
       Valid      │ a               1       20.0 
                  │ b               2       40.0 
                  │ c               2       40.0 
      ────────────┼──────────────────────────────
       Total      │                 5      100.0 
      
      Class: character
      Data: df
      Weight: w (rescaled)

# print.spicy_freq_table: valid = FALSE drops Valid Percent column

    Code
      freq(c(1, 2, 2, 3, 3, 3, NA), valid = FALSE)
    Output
      Frequency table: c(1, 2, 2, 3, 3, 3, NA)
      
       Category   │ Values      Freq.    Percent    Valid Percent 
      ────────────┼───────────────────────────────────────────────
       Valid      │ 1               1       14.3               NA 
                  │ 2               2       28.6               NA 
                  │ 3               3       42.9               NA 
       Missing    │ NA              1       14.3                  
      ────────────┼───────────────────────────────────────────────
       Total      │                 7      100.0            100.0 
      
      Class: numeric
      Data: c(1, 2, 2, 3, 3, 3, NA)

# print.spicy_freq_table: cumulative on a complete (no-NA) vector

    Code
      freq(c("a", "b", "b", "c", "c", "c"), cum = TRUE)
    Output
      Frequency table: c("a", "b", "b", "c", "c", "c")
      
       Category   │ Values      Freq.    Percent    Cum. Percent 
      ────────────┼──────────────────────────────────────────────
       Valid      │ a               1       16.7            16.7 
                  │ b               2       33.3            50.0 
                  │ c               3       50.0           100.0 
      ────────────┼──────────────────────────────────────────────
       Total      │                 6      100.0           100.0 
      
      Class: character
      Data: c("a", "b", "b", "c", "c", "c")

# print.spicy_freq_table: factor with unused declared level

    Code
      freq(f, factor_levels = "all")
    Output
      Frequency table: f
      
       Category   │ Values      Freq.    Percent 
      ────────────┼──────────────────────────────
       Valid      │ A               2       66.7 
                  │ B               1       33.3 
                  │ C               0        0.0 
      ────────────┼──────────────────────────────
       Total      │                 3      100.0 
      
      Class: factor
      Data: f

# print.spicy_cross_tab: vector mode, default

    Code
      cross_tab(x, y)
    Output
      Crosstable: x x y (N)
      
       Values   │   no    yes │   Total 
      ──────────┼─────────────┼─────────
       A        │    1      1 │       2 
       B        │    1      2 │       3 
       C        │    1      0 │       1 
      ──────────┼─────────────┼─────────
       Total    │    3      3 │       6 
      
      Chi-2(2) = 1.3, p = .513
      Cramer's V = 0.47
      Warning: 6 expected cells < 5 (100%). 2 expected cells < 1. Minimum expected = 0.5. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# print.spicy_cross_tab: data.frame with row percentages

    Code
      cross_tab(df, grp, out, percent = "row")
    Output
      Crosstable: grp x out (Row %)
      
       Values   │     hi      lo │   Total     N 
      ──────────┼────────────────┼───────────────
       A        │   50.0    50.0 │   100.0     4 
       B        │   50.0    50.0 │   100.0     4 
       C        │   50.0    50.0 │   100.0     4 
      ──────────┼────────────────┼───────────────
       Total    │   50.0    50.0 │   100.0    12 
      
      Chi-2(2) = 0.0, p = 1.000
      Cramer's V = 0.00
      Warning: 6 expected cells < 5 (100%). Minimum expected = 2. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# print.spicy_cross_tab: weighted + by interaction

    Code
      cross_tab(df, grp, out, by = sex, weights = w, percent = "column")
    Output
      Crosstable: grp x out (Column %) | sex = F
      
       Values   │      hi       lo │   Total 
      ──────────┼──────────────────┼─────────
       A        │    50.0     50.0 │    50.0 
       B        │    50.0     50.0 │    50.0 
      ──────────┼──────────────────┼─────────
       Total    │   100.0    100.0 │   100.0 
       N        │       2        6 │       8 
      
      Chi-2(1) = 0.0, p = 1.000
      Cramer's V = 0.00
      Warning: 4 expected cells < 5 (100%). Minimum expected = 1. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
      Weight: w
      
      Crosstable: grp x out (Column %) | sex = M
      
       Values   │      hi       lo │   Total 
      ──────────┼──────────────────┼─────────
       A        │    50.0     50.0 │    50.0 
       B        │    50.0     50.0 │    50.0 
      ──────────┼──────────────────┼─────────
       Total    │   100.0    100.0 │   100.0 
       N        │       2        6 │       8 
      
      Chi-2(1) = 0.0, p = 1.000
      Cramer's V = 0.00
      Warning: 4 expected cells < 5 (100%). Minimum expected = 1. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
      Weight: w

# print.spicy_categorical_table: single variable, no grouping

    Code
      table_categorical(df, select = "smoking")
    Output
      Categorical table
      
       Variable   │  n     %    
      ────────────┼─────────────
       smoking    │             
         No       │  7    70.0  
         Yes      │  3    30.0  

# print.spicy_continuous_table: numeric + group

    Code
      table_continuous(df, select = "age", by = sex)
    Output
      Descriptive statistics
      
       Variable │ Group    M     SD    Min    Max   95% CI LL  95% CI UL  n   p   
      ──────────┼─────────────────────────────────────────────────────────────────
       age      │ F      25.60  4.83  20.00  31.00    19.61      31.59    5  .001 
                │ M      41.60  5.41  35.00  50.00    34.88      48.32    5       

# print.spicy_categorical_table: by + auto assoc_measure (APA Note)

    Code
      table_categorical(df, select = c("smoking", "health"), by = grp)
    Output
      Categorical table by grp
      
       Variable │ F n  F %   M n  M %   Total n  Total %    p    Effect size 
      ──────────┼────────────────────────────────────────────────────────────
       smoking  │                                         1.000      .00     
         No     │  3   50.0   3   50.0     6      50.0                       
         Yes    │  3   50.0   3   50.0     6      50.0                       
      ╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       health   │                                         1.000      .00     
         low    │  2   33.3   2   33.3     4      33.3                       
         mid    │  2   33.3   2   33.3     4      33.3                       
         hi     │  2   33.3   2   33.3     4      33.3                       
      
      Note. Phi: smoking; Cramer's V: health.

# print.spicy_continuous_lm_table: bivariate fit by group

    Code
      table_continuous_lm(df, select = "score", by = sex)
    Output
      Continuous outcomes by sex
      
       Variable │ M (F)  M (M)  Δ (M - F)  95% CI LL  95% CI UL   p     R²   n  
      ──────────┼───────────────────────────────────────────────────────────────
       score    │ 70.66  75.26    4.60       -0.85      10.06    .092  0.19  16 

# print.spicy_assoc_table: omnibus 2x3 table

    Code
      assoc_measures(tab)
    Output
      Measure                            Estimate     SE  CI lower  CI upper     p 
      Cramer's V                            0.154     --     0.000     0.340  .304 
      Contingency Coefficient               0.152     --        --        --  .304 
      Lambda symmetric                      0.050  0.037     0.000     0.123  .179 
      Lambda R|C                            0.100  0.073     0.000     0.244  .174 
      Lambda C|R                            0.000  0.000     0.000     0.000    -- 
      Goodman-Kruskal's Tau R|C             0.024  0.030     0.000     0.082  .424 
      Goodman-Kruskal's Tau C|R             0.008  0.011     0.000     0.030  .454 
      Uncertainty Coefficient symmetric     0.014  0.018     0.000     0.050  .430 
      Uncertainty Coefficient R|C           0.017  0.022     0.000     0.061  .431 
      Uncertainty Coefficient C|R           0.012  0.015     0.000     0.042  .429 
      Goodman-Kruskal Gamma                 0.082  0.172    -0.255     0.419  .634 
      Kendall's Tau-b                       0.045  0.096    -0.142     0.233  .635 
      Kendall's Tau-c                       0.050  0.105    -0.156     0.256  .635 
      Somers' D R|C                         0.041  0.087    -0.129     0.212  .635 
      Somers' D C|R                         0.050  0.105    -0.156     0.256  .635 

# print.spicy_assoc_detail: cramer_v with CI

    Code
      cramer_v(tab, detail = TRUE)
    Output
      Estimate  CI lower  CI upper     p
         0.154     0.000     0.340  .304

