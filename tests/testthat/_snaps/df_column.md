# a table of z tests refuses the token with a classed error

    Code
      table_regression(g, show_columns = .dfc_cols)
    Condition
      Error in `table_regression()`:
      ! Token "df" in `show_columns` has no value for any model in the table: its tests are z tests, which have no degrees of freedom.
      i The column gives the degrees of freedom of t tests (`lm`, a `glm` whose dispersion is estimated, `nlme::lme`, a survey design, a cluster-robust `vcov = "CR2"`, ...).

