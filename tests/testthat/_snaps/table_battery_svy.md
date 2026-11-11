# the console prints the table in French

    Code
      table_battery_svy(des, tidyselect::all_of(.BSVY_ITEMS), by = stype, top = "Yes")
    Output
      Batterie d'items selon stype
      
       Variable   │  E Yes    E n    H Yes    H n    M Yes    M n      p     
      ────────────┼──────────────────────────────────────────────────────────
       awards     │  73,0     100    32,0     50     48,0     50     <0,001  
       sch.wide   │  91,0     100    52,0     50     70,0     50     <0,001  
       comp.imp   │  75,0     100    34,0     50     48,0     50     <0,001  
       both       │  73,0     100    32,0     50     48,0     50     <0,001  
      
      Note. Plan de sondage : stratifié (stype), avec correction pour population finie ; 197 degrés de liberté. Pourcentages en ligne estimés sous le plan de sondage. Base : personnes ayant répondu à l'item. Valeurs p des items, chaque item selon le groupe : khi-deux de Pearson fondé sur le plan de sondage (correction de Rao-Scott du second ordre). Ajustées pour 4 tests (méthode de Holm). n = effectif observé (non pondéré).

