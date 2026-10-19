# the console table is pinned

    Code
      print(.mr_farm_tbl())
    Output
      Multiple responses by ed
      
       Variable    │ HS n  HS %  VS n  VS %  2YC n  2YC %  4YC n  4YC %  Other n  Other %  Total n  Total %   p   
      ─────────────┼──────────────────────────────────────────────────────────────────────────────────────────────
       A           │  19   21.6    2   12.5    1     3.2     19   16.8      3      21.4       44     16.8    .447 
       B           │  38   43.2    6   37.5   13    41.9     29   25.7      4      28.6       90     34.4    .352 
       C           │  29   33.0    8   50.0   10    32.3     40   35.4      8      57.1       95     36.3    .658 
       D           │  47   53.4    8   50.0   17    54.8     53   46.9      6      42.9      131     50.0    .841 
       E           │  40   45.5    4   25.0   14    45.2     29   25.7      6      42.9       93     35.5    .123 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Respondents │  88          16          31            113            14                262                  
      
      Note. Multiple answers possible: percentages can sum to more than 100%. Base: respondents with data for the question. Item p-values: Pearson chi-squared test (C, D); Fisher exact test (A, B, E). P-values adjusted for 5 tests (Holm method). Overall group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(18.60) = 28.68, p = .063.

---

    Code
      print(table_multiresponse(.mr_toy(), c(x1, x2, x3, x4), by = g, percent = "both", statistic = TRUE))
    Output
      Multiple responses by g
      
       Variable    │ a n  a % of cases  a % of responses  b n  b % of cases  b % of responses  Total n  Total % of cases 
      ─────────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────
       x1          │  2       66.7            40.0         1       33.3            14.3           3           50.0       
       x2          │  0        0.0             0.0         3      100.0            42.9           3           50.0       
       x3          │  0        0.0             0.0         0        0.0             0.0           0            0.0       
       x4          │  3      100.0            60.0         3      100.0            42.9           6          100.0       
      ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Respondents │  3                                    3                                      6                      
      
       Variable    │ Total % of responses  Test    p   
      ─────────────┼───────────────────────────────────
       x1          │         25.0           –    1.000 
       x2          │         25.0           –     .200 
       x3          │          0.0           –     –    
       x4          │         50.0           –     –    
      ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Respondents │                                   
      
      Note. Rows with missing g removed: 1. Multiple answers possible: percentages of cases can sum to more than 100%, and percentages of responses sum to 100%. Base: respondents with data for the question (excluded: 1 with no data for the question). Item p-values: Fisher exact test of each item by group. P-values adjusted for 2 tests (Holm method). Not tested (no variation, shown as –): x3, x4. Overall group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(5.06) = 16.87, p = .005.

---

    Code
      print(table_multiresponse(.mr_toy(), c(x1, x2, x3), by = g, none_row = FALSE))
    Output
      Réponses multiples selon g
      
       Variable    │  a n    a %     b n     b %     Total n    Total %      p    
      ─────────────┼──────────────────────────────────────────────────────────────
       x1          │   2     66,7     1      33,3       3        50,0      1,000  
       x2          │   0      0,0     3     100,0       3        50,0      0,200  
       x3          │   0      0,0     0       0,0       0         0,0       –     
      ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Personnes   │   3              3                 6                         
      
      Note. Lignes retirées pour valeur manquante sur g : 1. Plusieurs réponses possibles : les pourcentages peuvent totaliser plus de 100 %. Base : personnes ayant des données pour la question, dont 1 sans aucun item coché (hors base : 1 sans donnée pour la question). Valeurs p des items : test exact de Fisher de chaque item selon le groupe. Valeurs p ajustées pour 2 tests (méthode de Holm). Non testé (sans variation, signalé par –) : x3. Comparaison globale des groupes : test du khi-deux d'indépendance marginale multiple avec correction de Rao-Scott du second ordre, χ²(5,06) = 16,87, p = 0,005.

---

    Code
      print(table_multiresponse(.mr_toy(), c(x1, x2, x3), by = g))
    Output
      Réponses multiples selon g
      
       Variable             │  a n    a %     b n     b %     Total n    Total %      p    
      ──────────────────────┼──────────────────────────────────────────────────────────────
       x1                   │   2     66,7     1      33,3       3        50,0      1,000  
       x2                   │   0      0,0     3     100,0       3        50,0      0,200  
       x3                   │   0      0,0     0       0,0       0         0,0       –     
       (Aucun item coché)   │   1     33,3     0       0,0       1        16,7       –     
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Personnes            │   3              3                 6                         
      
      Note. Lignes retirées pour valeur manquante sur g : 1. Plusieurs réponses possibles : les pourcentages peuvent totaliser plus de 100 %. Base : personnes ayant des données pour la question (hors base : 1 sans donnée pour la question). Valeurs p des items : test exact de Fisher de chaque item selon le groupe. Valeurs p ajustées pour 2 tests (méthode de Holm). Non testé (sans variation, signalé par –) : x3. Comparaison globale des groupes : test du khi-deux d'indépendance marginale multiple avec correction de Rao-Scott du second ordre, χ²(5,06) = 16,87, p = 0,005.

---

    Code
      print(table_multiresponse(.mr_none(), c(x1, x2), by = g, base = "all", percent = "both", statistic = TRUE, test = "chisq"))
    Output
      Multiple responses by g
      
       Variable                   │ a n  a % of cases  a % of responses  b n  b % of cases  b % of responses  Total n 
      ────────────────────────────┼───────────────────────────────────────────────────────────────────────────────────
       x1                         │  1       33.3           100.0         2       66.7            66.7           3    
       x2                         │  0        0.0             0.0         1       33.3            33.3           1    
       (No item chosen)           │  1       33.3              –          1       33.3              –            2    
       (No data for the question) │  1       33.3              –          0        0.0              –            1    
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Respondents                │  3                                    3                                      6    
      
       Variable                   │ Total % of cases  Total % of responses      Test        p   
      ────────────────────────────┼─────────────────────────────────────────────────────────────
       x1                         │       50.0                75.0          χ²(1) = 0.67   .547 
       x2                         │       16.7                25.0          χ²(1) = 1.20   .547 
       (No item chosen)           │       33.3                  –                    –     –    
       (No data for the question) │       16.7                  –                    –     –    
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Respondents                │                                                             
      
      Note. Rows with missing g removed: 1. Multiple answers possible: percentages of cases can sum to more than 100%, and percentages of responses sum to 100%. Base: all respondents. Item p-values: Pearson chi-squared test of each item by group. P-values adjusted for 2 tests (Holm method). Overall group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(2.49) = 2.32, p = .410.

