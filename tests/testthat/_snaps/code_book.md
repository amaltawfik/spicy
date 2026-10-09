# the print is pinned

    Code
      print(cb)
    Output
      Codebook
      Wave 1
      Jane Doe – HESAV
      Bob
      
      Date: 2026-10-07
      Observations: 6
      Variables: 6
      Declared missing value: 8 = DK (2 variables)
      Declared missing value: 9 = Refused (1 variable)
      Declared missing value: NA(a) = Refused (1 variable)
      Note: Counts and percentages are unweighted: they describe the data file and are not estimates for a population.
      Note: Fictitious data.
      Note: - Marked note.
      
         Pos. │ Variable    Label           Type                              Valid    Missing 
      ────────┼────────────────────────────────────────────────────────────────────────────────
            1 │ sex         Sex             categorical (nominal)                 5          1 
            2 │ score       Score (0-20)    numeric                               5          1 
            3 │ day                         date                                  5          1 
            4 │ q1                          categorical (labelled codes)          3          3 
            5 │ q2                          categorical (labelled codes)          4          2 
            6 │ q3                          categorical (labelled codes)          4          2 

---

    Code
      print(cb)
    Output
      Codebook
      Jane Doe – HESAV
      
      Date : 2026-10-07
      Observations : 6
      Variables : 2
      Valeur manquante déclarée : 8 = DK (1 variable)
      Valeur manquante déclarée : 9 = Refused (1 variable)
      Note : Les effectifs et les pourcentages ne sont pas pondérés : ils décrivent le fichier de données et ne sont pas des estimations pour la population.
      
         Pos. │ Variable    Libellé    Type                                Valides    Manquants 
      ────────┼─────────────────────────────────────────────────────────────────────────────────
            1 │ sex         Sex        catégorielle (nominale)                   5            1 
            4 │ q1                     catégorielle (codes étiquetés)            3            3 

