# snapshot — single lm default rendering

    Code
      cat(capture_norm(out))
    Output
      Regression: mpg
      
       Variable      │    B       SE         95% CI          p
      ───────────────┼────────────────────────────────────────────
       (Intercept)   │   33.99    1.89    [30.12, 37.86]    <.001
       wt            │   -3.21    0.75    [-4.75, -1.66]    <.001
       cyl:          │
         4 (ref.)    │    —       —             —           —
         6           │   -4.26    1.39    [-7.09, -1.42]     .005
         8           │   -6.07    1.65    [-9.46, -2.69]    <.001
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n             │   32
       R²            │    0.84
       Adj.R²        │    0.82
      
      Note. Std. errors: classical (OLS).

# snapshot — multi-model with nested = TRUE comparison footer

    Code
      cat(capture_norm(out))
    Output
      Hierarchical regression: mpg
      
       Variable      │  Model 1: B    Model 1: SE    Model 1: 95% CI    Model 1: p
      ───────────────┼──────────────────────────────────────────────────────────────
       (Intercept)   │        37.29           1.88     [33.45, 41.12]         <.001
       wt            │        -5.34           0.56     [-6.49, -4.20]         <.001
       cyl:          │
         4 (ref.)    │         —              —              —                —
         6           │
         8           │
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n             │        32
       R²            │         0.75
       Adj.R²        │         0.74
      
       Variable      │  Model 2: B    Model 2: SE    Model 2: 95% CI    Model 2: p
      ───────────────┼──────────────────────────────────────────────────────────────
       (Intercept)   │        33.99           1.89     [30.12, 37.86]         <.001
       wt            │        -3.21           0.75     [-4.75, -1.66]         <.001
       cyl:          │
         4 (ref.)    │         —              —              —                —
         6           │        -4.26           1.39     [-7.09, -1.42]          .005
         8           │        -6.07           1.65     [-9.46, -2.69]         <.001
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n             │        32
       R²            │         0.84
       Adj.R²        │         0.82
      
      Note. Std. errors: classical (OLS).
      
      ── Model comparison ──
      Model 2 vs Model 1: ΔR² = +0.08, F = +7.29, p = .003

# snapshot — standardized + stars + reference annotation

    Code
      cat(capture_norm(out))
    Output
      Regression: mpg
      
       Variable        │    B         β         SE         95% CI          p
      ─────────────────┼────────────────────────────────────────────────────────
       (Intercept)     │   33.99     0.60**     1.89    [30.12, 37.86]    <.001
       wt              │   -3.21    -0.52***    0.75    [-4.75, -1.66]    <.001
       cyl: [ref: 4]   │
         6             │   -4.26    -0.71**     1.39    [-7.09, -1.42]     .005
         8             │   -6.07    -1.01***    1.65    [-9.46, -2.69]    <.001
      ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
       n               │   32
       R²              │    0.84
       Adj.R²          │    0.82
      
      Note. Std. errors: classical (OLS).
      *** p < .001, ** p < .01, * p < .05.

