# a French table of each family is pinned

    Code
      print(freq(d$smoke))
    Output
      Tableau de fréquences : smoke
      
       Catégorie   │ Valeurs      Eff.    Pourcentage    Pourcentage valide 
      ─────────────┼────────────────────────────────────────────────────────
       Valide      │ No              4           50.0                  57.1 
                   │ Yes             3           37.5                  42.9 
       Manquant    │ NA              1           12.5                       
      ─────────────┼────────────────────────────────────────────────────────
       Total       │                 8          100.0                 100.0 
      
      Classe : factor
      Données : d
    Code
      print(cross_tab(d, smoke, sex, percent = "column"))
    Output
      Tableau croisé : smoke x sex (% colonne)
      
       Values   │       F        M │   Total 
      ──────────┼──────────────────┼─────────
       No       │    50.0     66.7 │    57.1 
       Yes      │    50.0     33.3 │    42.9 
      ──────────┼──────────────────┼─────────
       Total    │   100.0    100.0 │   100.0 
       N        │       4        3 │       7 
      
      Khi-2(1) = 0.2, p = .659
      V de Cramér = 0.17
      Avertissement : 4 cellules avec effectif théorique < 5 (100 %). Effectif théorique minimum = 1.29. Envisagez `simulate_p = TRUE` ou définissez-le globalement via `options(spicy.simulate_p = TRUE)`.
      Valeurs manquantes retirées : smoke (1).
    Code
      print(table_categorical(d, select = smoke, by = sex))
    Output
      Tableau des variables catégorielles selon sex
      
       Variable       │  F n    F %     M n    M %     Total n    Total %     p      Phi  
      ────────────────┼───────────────────────────────────────────────────────────────────
       smoke          │                                                      .659    .17  
         No           │   2     50.0     2     50.0       4        50.0                   
         Yes          │   2     50.0     1     25.0       3        37.5                   
         (Manquant)   │   0      0.0     1     25.0       1        12.5                   
      Tableau des variables catégorielles selon sex
      
       Variable       │  F n    F %     M n    M %     Total n    Total %     p      Phi  
      ────────────────┼───────────────────────────────────────────────────────────────────
       smoke          │                                                      .659    .17  
         No           │   2     50.0     2     50.0       4        50.0                   
         Yes          │   2     50.0     1     25.0       3        37.5                   
         (Manquant)   │   0      0.0     1     25.0       1        12.5                   
    Code
      print(table_continuous(d, select = c(bmi, age), by = sex))
    Output
      Statistiques descriptives selon sex
      
       Variable   │ Groupe       M       ET       Min      Max     95% CI LL    95% CI UL    n     p    
      ────────────┼─────────────────────────────────────────────────────────────────────────────────────
       bmi        │ F          26.88     3.69    22.10    31.00      21.01        32.74      4    .152  
                  │ M          23.18     2.41    19.80    25.40      19.34        27.01      4          
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       age        │ F          48.25    12.58    31.00    61.00      28.23        68.27      4    .153  
                  │ M          35.50     8.74    24.00    45.00      21.60        49.40      4          
      Statistiques descriptives selon sex
      
       Variable   │ Groupe       M       ET       Min      Max     95% CI LL    95% CI UL    n     p    
      ────────────┼─────────────────────────────────────────────────────────────────────────────────────
       bmi        │ F          26.88     3.69    22.10    31.00      21.01        32.74      4    .152  
                  │ M          23.18     2.41    19.80    25.40      19.34        27.01      4          
      ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       age        │ F          48.25    12.58    31.00    61.00      28.23        68.27      4    .153  
                  │ M          35.50     8.74    24.00    45.00      21.60        49.40      4          
    Code
      print(table_continuous_lm(d, select = bmi, by = sex))
    Output
      Variables continues selon sex
      
       Variable   │  M (F)    M (M)    Δ (M - F)    95% CI LL    95% CI UL     p       R²     n  
      ────────────┼──────────────────────────────────────────────────────────────────────────────
       bmi        │  26.87    23.17      -3.70        -9.09        1.69       .144    0.32    8  
      Variables continues selon sex
      
       Variable   │  M (F)    M (M)    Δ (M - F)    95% CI LL    95% CI UL     p       R²     n  
      ────────────┼──────────────────────────────────────────────────────────────────────────────
       bmi        │  26.87    23.17      -3.70        -9.09        1.69       .144    0.32    8  
    Code
      print(table_outcome(d, outcome = bmi, by = c(sex, smoke)))
    Output
      Statistiques descriptives pour bmi
      
       Variable       │    M       ET      Min      Max     95% CI LL    95% CI UL    n     p    
      ────────────────┼──────────────────────────────────────────────────────────────────────────
       Ensemble       │  25.02    3.50    19.80    31.00      22.10        27.95      8          
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       sex            │                                                                    .152  
         F            │  26.88    3.69    22.10    31.00      21.01        32.74      4          
         M            │  23.18    2.41    19.80    25.40      19.34        27.01      4          
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       smoke          │                                                                    .559  
         No           │  26.00    4.73    19.80    31.00      18.48        33.52      4          
         Yes          │  24.30    2.25    22.10    26.60      18.71        29.89      3          
         (Manquant)   │  23.30     –      23.30    23.30        –            –        1          
      
      Comparaison des groupes : test t de Welch. Chaque bloc compare bmi entre les modalités d'une variable ; les blocs ne sont pas ajustés les uns pour les autres. Ensemble = l'échantillon analytique complet.
    Code
      print(table_regression(stats::lm(bmi ~ age + sex, data = d)))
    Output
      Linear regression: bmi
      
       Variable    │   B     SE       95% CI        p   
      ─────────────┼────────────────────────────────────
       (Intercept) │ 13.12  0.81  [11.02, 15.21]  <.001 
       age         │  0.29  0.02  [ 0.24,  0.33]  <.001 
       sex :       │                                    
         F (ref.)  │   –     –          –          –    
         M         │ -0.06  0.37  [-1.01,  0.88]   .869 
      ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n           │  8                                 
       R²          │  0.99                              
       Adj. R²     │  0.98                              
      
      Note. Linear regression.
      Erreurs types : classiques (MCO).

# a single overridden label is pinned

    Code
      print(table_categorical(d, select = sex, by = arm))
    Output
      Categorical table by arm
      
       Variable │ Campus n  Campus %  Control n  Control %  (No answer) n  (No answer) %  All n  All % 
      ──────────┼──────────────────────────────────────────────────────────────────────────────────────
       sex      │                                                                                      
         F      │    0         0.0        2        100.0          1            50.0         3    50.0  
         M      │    2       100.0        0          0.0          1            50.0         3    50.0  
      
       Variable │  p    Phi  
      ──────────┼────────────
       sex      │ .046  1.00 
         F      │            
         M      │            
      Categorical table by arm
      
       Variable │ Campus n  Campus %  Control n  Control %  (No answer) n  (No answer) %  All n  All % 
      ──────────┼──────────────────────────────────────────────────────────────────────────────────────
       sex      │                                                                                      
         F      │    0         0.0        2        100.0          1            50.0         3    50.0  
         M      │    2       100.0        0          0.0          1            50.0         3    50.0  
      
       Variable │  p    Phi  
      ──────────┼────────────
       sex      │ .046  1.00 
         F      │            
         M      │            

