# Numerical validation of `R/assoc.R`

This directory contains the cross-validation pipeline for the 13
association measures exported by `spicy::*`. It is versioned for
transparency and reproducibility but is excluded from the CRAN
build (see `.Rbuildignore`).

## What is here

| File | Purpose |
|------|---------|
| `REPORT.md` | Human-readable validation report with the full convergence matrix |
| `gen_sps.R` | Generates `.sps` syntax files from R datasets |
| `mtcars_3x3.sps`, `mtcars_2x2.sps`, `HairEye.sps`, `sochealth.sps` | Frozen PSPP CROSSTABS inputs |
| `mtcars_3x3_pspp.csv`, `mtcars_2x2_pspp.csv`, `HairEye_pspp.csv`, `sochealth_pspp.csv` | Frozen PSPP outputs (the oracle) |
| `validate.R` | Multi-package R triangulation (DescTools, vcd, Hmisc) |
| `compare_pspp.R` | Builds the spicy-vs-PSPP convergence matrix |
| `spicy_vs_pspp.csv` | Flat snapshot of the comparison |

The PSPP-derived oracle values are also pinned in
`tests/testthat/test-assoc.R` under the four `test_that("PSPP oracle:
...")` blocks. Those tests are the load-bearing anti-regression
guards; this directory documents how the values were obtained.

## Reproducibility

### Dependencies

* PSPP 2.0+ (https://www.gnu.org/software/pspp/) on the `PATH` or at
  `C:\Program Files\PSPP\bin\pspp.exe` on Windows
* R 4.1+ with `devtools`, `DescTools`, `vcd`, `Hmisc` (CRAN)

### Recipe

From the package root:

```sh
# 1. Regenerate the .sps inputs from R datasets (overwrites *.sps)
Rscript tools/validation/gen_sps.R

# 2. Run PSPP on each dataset (overwrites *_pspp.csv)
for f in mtcars_3x3 mtcars_2x2 HairEye sochealth; do
  pspp -O format=csv -o tools/validation/${f}_pspp.csv \
       tools/validation/${f}.sps
done

# 3. Run the multi-package R triangulation (DescTools, vcd, Hmisc)
Rscript tools/validation/validate.R

# 4. Build the spicy-vs-PSPP comparison (overwrites spicy_vs_pspp.csv)
Rscript tools/validation/compare_pspp.R
```

After re-running, inspect the diff for `spicy_vs_pspp.csv`. If the
PSPP-derived test block in `tests/testthat/test-assoc.R` would need
to change, that is a signal that an `R/assoc.R` change altered point
estimates: investigate before pinning new values.

## When to update

* **A new association measure is added to `R/assoc.R`** — extend the
  `compute_spicy()` and oracle dictionaries in `compare_pspp.R`,
  re-run, regenerate the report.
* **A formula in `R/assoc.R` changes** — re-run the pipeline and
  manually verify the diff in `spicy_vs_pspp.csv` is intentional
  before updating the test oracle.
* **A new dataset would surface an edge case** — add it to
  `gen_sps.R`, run PSPP, add a test block.

Routine maintenance does not require re-running the pipeline; the
pinned tests in `test-assoc.R` provide ongoing protection.
