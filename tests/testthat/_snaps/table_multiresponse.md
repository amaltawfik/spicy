# the console table is pinned

    Code
      print(.mr_farm_tbl())
    Output
      Multiple responses by ed
      
       Variable    │ HS n  HS %  VS n  VS %  2YC n  2YC %  4YC n  4YC %  Other n  Other %  Total n  Total %   p   
      ─────────────┼──────────────────────────────────────────────────────────────────────────────────────────────
       A           │  19   21.6    2   12.5    1     3.2     19   16.8      3      21.4       44     16.8    .607 
       B           │  38   43.2    6   37.5   13    41.9     29   25.7      4      28.6       90     34.4    .382 
       C           │  29   33.0    8   50.0   10    32.3     40   35.4      8      57.1       95     36.3    .658 
       D           │  47   53.4    8   50.0   17    54.8     53   46.9      6      42.9      131     50.0    .841 
       E           │  40   45.5    4   25.0   14    45.2     29   25.7      6      42.9       93     35.5    .136 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Respondents │  88          16          31            113            14                262                  
      
      Several answers possible: percentages are of respondents and can sum to more than 100%. Base: respondents who answered the question, including those who chose no item. Item p-values: Pearson chi-squared test of each item by group. P-values adjusted via stats::p.adjust(method = "holm"); m = 5 test(s). Group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(18.60) = 28.68, p = .063.

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
      
       Variable    │ Total % of responses      Test        p   
      ─────────────┼───────────────────────────────────────────
       x1          │         25.0          χ²(1) = 0.67   .414 
       x2          │         25.0          χ²(1) = 6.00   .029 
       x3          │          0.0                   –     –    
       x4          │         50.0                   –     –    
      ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Respondents │                                           
      
      Rows with missing g removed: 1. Respondents with no answer to the question removed: 1. Several answers possible: percentages of respondents can sum to more than 100%; percentages of responses sum to 100%. Base: respondents who answered the question, including those who chose no item. Item p-values: Pearson chi-squared test of each item by group. P-values adjusted via stats::p.adjust(method = "holm"); m = 2 test(s). Not tested (no variation, shown as –): x3, x4. Group comparison: Rao-Scott second-order corrected chi-squared test of multiple marginal independence, χ²(5.06) = 16.87, p = .005.

---

    Code
      print(table_multiresponse(.mr_toy(), c(x1, x2, x3), by = g))
    Output
      Réponses multiples selon g
      
       Variable                  │  a n    a %     b n     b %     Total n    Total %      p    
      ───────────────────────────┼──────────────────────────────────────────────────────────────
       x1                        │   2     66,7     1      33,3       3        50,0      0,414  
       x2                        │   0      0,0     3     100,0       3        50,0      0,029  
       x3                        │   0      0,0     0       0,0       0         0,0       –     
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Personnes ayant répondu   │   3              3                 6                         
      
      Lignes retirées pour valeur manquante sur g : 1. Personnes retirées faute de réponse à la question : 1. Plusieurs réponses possibles : les pourcentages, calculés sur les personnes ayant répondu, peuvent totaliser plus de 100 %. Base : personnes ayant répondu à la question, y compris celles qui n'ont choisi aucun item. 1 personne n'a coché aucun item. Valeurs p des items : test du khi-deux de Pearson de chaque item selon le groupe. Valeurs p ajustées par stats::p.adjust(method = "holm") ; m = 2 test(s). Non testé (sans variation, signalé par –) : x3. Comparaison des groupes : test du khi-deux d'indépendance marginale multiple avec correction de Rao-Scott du second ordre, χ²(5,06) = 16,87, p = 0,005.

