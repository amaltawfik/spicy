# snapshot: glmmTMB ordinal table

    Code
      cat(paste(sub("[ \t]+$", "", txt), collapse = "\n"))
    Output
      Cumulative logit mixed-effects regression (proportional odds) (glmmTMB): rating
      
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
      
      Note. Cumulative logit mixed-effects regression (proportional odds) (glmmTMB).
      Std. errors: Wald asymptotic (z).
      p-values: Wald-z asymptotic (glmmTMB).
      Random effects (ML): LR test vs cumulative logit regression, χ̄²(1) = 9.85, p < .001.
      Thresholds: latent-scale category cut-points.

