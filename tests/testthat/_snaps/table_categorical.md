# p_adjust under holm is pinned, in English and in French

    Code
      print(table_categorical(mtcars, c(cyl, gear, vs), by = am, p_adjust = "holm"))
    Output
      Categorical table by am
      
       Variable   │  1 n    1 %     0 n    0 %     Total n    Total %      p      Effect size  
      ────────────┼────────────────────────────────────────────────────────────────────────────
       cyl        │                                                       .025        .52      
         6        │   3     23.1     4     21.1       7        21.9                            
         4        │   8     61.5     3     15.8      11        34.4                            
         8        │   2     15.4    12     63.2      14        43.8                            
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       gear       │                                                      <.001        .81      
         4        │   8     61.5     4     21.1      12        37.5                            
         3        │   0      0.0    15     78.9      15        46.9                            
         5        │   5     38.5     0      0.0       5        15.6                            
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       vs         │                                                       .341        .17      
         0        │   6     46.2    12     63.2      18        56.2                            
         1        │   7     53.8     7     36.8      14        43.8                            
      
      Note. Cramer's V: cyl, gear; Phi: vs. P-values adjusted via stats::p.adjust(method = "holm"); m = 3 test(s).

---

    Code
      print(table_categorical(mtcars, c(cyl, gear, vs), by = am, p_adjust = "holm"))
    Output
      Tableau des variables catégorielles selon am
      
       Variable   │  1 n    1 %     0 n    0 %     Total n    Total %      p       Taille d'effet  
      ────────────┼────────────────────────────────────────────────────────────────────────────────
       cyl        │                                                       0,025         0,52       
         6        │   3     23,1     4     21,1       7        21,9                                
         4        │   8     61,5     3     15,8      11        34,4                                
         8        │   2     15,4    12     63,2      14        43,8                                
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       gear       │                                                      <0,001         0,81       
         4        │   8     61,5     4     21,1      12        37,5                                
         3        │   0      0,0    15     78,9      15        46,9                                
         5        │   5     38,5     0      0,0       5        15,6                                
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       vs         │                                                       0,341         0,17       
         0        │   6     46,2    12     63,2      18        56,2                                
         1        │   7     53,8     7     36,8      14        43,8                                
      
      Note. V de Cramér : cyl, gear; Phi : vs. Valeurs p ajustées par stats::p.adjust(method = "holm") sur m = 3 test(s).

