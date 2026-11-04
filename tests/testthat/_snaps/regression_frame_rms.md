# snapshot: ordinal lrm table, default and exponentiated

    Code
      cat(.rms_snap_text(t_b))
    Output
      Cumulative logit regression (proportional odds): gear_o
      
       Variable        │   B     SE       95% CI         p
      ─────────────────┼─────────────────────────────────────
       wt              │ -3.31  0.99  [ -5.24, -1.38]  <.001
       hp              │  0.02  0.01  [  0.00,  0.04]   .016
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Thresholds:     │
         3 | 4         │ -7.69  2.39  [-12.37, -3.02]   .001
         4 | 5         │ -4.49  1.88  [ -8.18, -0.80]   .017
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │ 32
       R² (McFadden)   │  0.33
       R² (Nagelkerke) │  0.56
       AIC             │ 51.5
      
      Note. Cumulative logit regression (proportional odds). Std. errors: Wald asymptotic (z). Thresholds: latent-scale category cut-points. CI = confidence interval; AIC = Akaike information criterion.

---

    Code
      cat(.rms_snap_text(t_or))
    Output
      Cumulative logit regression (proportional odds): gear_o
      
       Variable        │  OR     SE       95% CI         p
      ─────────────────┼─────────────────────────────────────
       wt              │  0.04  0.04  [  0.01,  0.25]  <.001
       hp              │  1.02  0.01  [  1.00,  1.04]   .016
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Thresholds:     │
         3 | 4         │ -7.69  2.39  [-12.37, -3.02]   .001
         4 | 5         │ -4.49  1.88  [ -8.18, -0.80]   .017
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │ 32
       R² (McFadden)   │  0.33
       R² (Nagelkerke) │  0.56
       AIC             │ 51.5
      
      Note. Cumulative logit regression (proportional odds). Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric). Std. errors: Wald asymptotic (z). Thresholds: latent-scale category cut-points (log-odds scale, not exponentiated). CI = confidence interval; AIC = Akaike information criterion.

