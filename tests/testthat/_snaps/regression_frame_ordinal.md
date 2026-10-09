# snapshot: clmm table, default and exponentiated

    Code
      cat(.snap_text(table_regression(fit)))
    Output
      Cumulative logit mixed-effects regression (proportional odds): rating
      
       Variable              │   B      SE       95% CI        p
      ───────────────────────┼─────────────────────────────────────
       temp:                 │
         cold (ref.)         │    –     –          –          –
         warm                │   3.06  0.60  [ 1.90,  4.23]  <.001
       contact:              │
         no (ref.)           │    –     –          –          –
         yes                 │   1.83  0.51  [ 0.83,  2.84]  <.001
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Thresholds:           │
         1 | 2               │  -1.62  0.68  [-2.96, -0.29]   .017
         2 | 3               │   1.51  0.60  [ 0.33,  2.70]   .012
         3 | 4               │   4.23  0.81  [ 2.64,  5.81]  <.001
         4 | 5               │   6.09  0.97  [ 4.18,  7.99]  <.001
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Random effects:       │
         σ judge (Intercept) │   1.13  0.43  [ 0.57,  2.26]   –
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                     │  72
       N (judge)             │   9
       AIC                   │ 177.1
       BIC                   │ 193.1
      
      Note. Cumulative logit mixed-effects regression (proportional odds).
      Std. errors: Wald asymptotic (z).
      Random effects (ML): LR test vs cumulative logit regression, χ̄²(1) = 9.85, p < .001.
      Thresholds: latent-scale category cut-points.

---

    Code
      cat(.snap_text(table_regression(fit, exponentiate = TRUE)))
    Output
      Cumulative logit mixed-effects regression (proportional odds): rating
      
       Variable              │   OR     SE        95% CI        p
      ───────────────────────┼──────────────────────────────────────
       temp:                 │
         cold (ref.)         │    –      –          –          –
         warm                │  21.39  12.74  [ 6.66, 68.71]  <.001
       contact:              │
         no (ref.)           │    –      –          –          –
         yes                 │   6.26   3.21  [ 2.29, 17.11]  <.001
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Thresholds:           │
         1 | 2               │  -1.62   0.68  [-2.96, -0.29]   .017
         2 | 3               │   1.51   0.60  [ 0.33,  2.70]   .012
         3 | 4               │   4.23   0.81  [ 2.64,  5.81]  <.001
         4 | 5               │   6.09   0.97  [ 4.18,  7.99]  <.001
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       Random effects:       │
         σ judge (Intercept) │   1.13   0.43  [ 0.57,  2.26]   –
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n                     │  72
       N (judge)             │   9
       AIC                   │ 177.1
       BIC                   │ 193.1
      
      Note. Cumulative logit mixed-effects regression (proportional odds).
      Std. errors: Wald asymptotic (z).
      Random effects (ML): LR test vs cumulative logit regression, χ̄²(1) = 9.85, p < .001.
      Thresholds: latent-scale category cut-points (log-odds scale, not exponentiated).
      OR = odds ratio.
      Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric).

