# p_adjust under holm is pinned, in English and in French

    Code
      print(table_categorical(mtcars, c(cyl, gear, vs), by = am, p_adjust = "holm"))
    Output
      Categorical table by am
      
       Variable   │  0 n    0 %     1 n    1 %     Total n    Total %      p      Effect size  
      ────────────┼────────────────────────────────────────────────────────────────────────────
       cyl        │                                                       .018        .52      
         4        │   3     15.8     8     61.5      11        34.4                            
         6        │   4     21.1     3     23.1       7        21.9                            
         8        │  12     63.2     2     15.4      14        43.8                            
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       gear       │                                                      <.001        .81      
         3        │  15     78.9     0      0.0      15        46.9                            
         4        │   4     21.1     8     61.5      12        37.5                            
         5        │   0      0.0     5     38.5       5        15.6                            
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       vs         │                                                       .341        .17      
         0        │  12     63.2     6     46.2      18        56.2                            
         1        │   7     36.8     7     53.8      14        43.8                            
      
      Note. Cramer's V: cyl, gear. Phi: vs. Group comparison: Pearson chi-squared test (vs); Fisher exact test (cyl, gear). P-values adjusted for 3 tests (Holm method).

---

    Code
      print(table_categorical(mtcars, c(cyl, gear, vs), by = am, p_adjust = "holm"))
    Output
      Tableau des variables catégorielles selon am
      
       Variable   │  0 n    0 %     1 n    1 %     Total n    Total %      p       Taille d'effet  
      ────────────┼────────────────────────────────────────────────────────────────────────────────
       cyl        │                                                       0,018         0,52       
         4        │   3     15,8     8     61,5      11        34,4                                
         6        │   4     21,1     3     23,1       7        21,9                                
         8        │  12     63,2     2     15,4      14        43,8                                
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       gear       │                                                      <0,001         0,81       
         3        │  15     78,9     0      0,0      15        46,9                                
         4        │   4     21,1     8     61,5      12        37,5                                
         5        │   0      0,0     5     38,5       5        15,6                                
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       vs         │                                                       0,341         0,17       
         0        │  12     63,2     6     46,2      18        56,2                                
         1        │   7     36,8     7     53,8      14        43,8                                
      
      Note. V de Cramér : cyl, gear. Phi : vs. Comparaison des groupes : test du khi-deux de Pearson (vs) ; test exact de Fisher (cyl, gear). Valeurs p ajustées pour 3 tests (méthode de Holm).

# p_value = FALSE with assoc_measure = "none": a purely descriptive table

    Code
      print(tbl)
    Output
      Categorical table by sex
      
       Variable            │  Female n    Female %    Male n    Male %    Total n    Total %  
      ─────────────────────┼──────────────────────────────────────────────────────────────────
       smoking             │                                                                  
         No                │    475         76.6       451       77.8       926       77.2    
         Yes               │    131         21.1       118       20.3       249       20.8    
         (Missing)         │     14          2.3        11        1.9        25        2.1    
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       education           │                                                                  
         Lower secondary   │    134         21.6       127       21.9       261       21.8    
         Upper secondary   │    290         46.8       249       42.9       539       44.9    
         Tertiary          │    196         31.6       204       35.2       400       33.3    

