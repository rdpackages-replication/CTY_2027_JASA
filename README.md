# Cattaneo, Titiunik and Yu (2027, JASA)

Replication files for "Estimation and Inference in Boundary Discontinuity
Designs: Location-Based Methods."

The R scripts reproduce the empirical and simulation results reported in the
paper and supplemental appendix. Python and Stata scripts provide additional
implementations of the empirical analysis. If running more than one software
implementation, use a separate copy of this folder for each language, as the
scripts overwrite files in `output/`, `tables/`, and `figures/` (created
automatically).

## Contents and Verification

The replication archive contains the repository files, including all eleven
analysis/display scripts, `spp.csv`, this README, and the tested `rd2d` source
in `software/rd2d-4d8490e6.tar.gz`. It excludes Git history, local workflow
notes, and generated analysis outputs. `SHA256SUMS` records every included
file except itself. From the replication folder, verify the contents with:

```sh
sha256sum -c SHA256SUMS
```

The package source is a snapshot of `rdpackages/rd2d` at commit
`4d8490e6dc32ba83450d61a9a924ee0af85dbc93`, including its original license.
Use this package snapshot to reproduce the results in the paper; software and
package versions are listed in [`software/versions.txt`](software/versions.txt).

## License

The eleven replication scripts in this folder are distributed under the
[MIT License](LICENSE). This license does not apply to `spp.csv` or the
bundled `rd2d` source, which retains its existing GPL-3 license.

## Data

The empirical application uses the Ser Pilo Paga data from:

- Londono-Velez, J., C. Rodriguez, and F. Sanchez (2020):
  [Upstream and Downstream Impacts of College Merit-Based Financial Aid for
  Low-Income Students: Ser Pilo Paga in Colombia](https://doi.org/10.1257/pol.20180131),
  _American Economic Journal: Economic Policy_ 12(2): 193-227.

[`spp.csv`](spp.csv) contains 363,096 students from the 2014 cohort. The analysis
file retains observations with both centered scores observed. Each row is one
student; the six columns are:

| Column | Definition and units |
| --- | --- |
| `running_saber11` | SABER 11 test score relative to its eligibility cutoff, in test-score points. Nonnegative values satisfy the academic requirement. |
| `running_sisben` | Signed distance from the SISBEN eligibility cutoff, in index points. Nonnegative values satisfy the wealth requirement. |
| `eligible_spp` | Eligibility indicator: 1 if both centered scores are nonnegative, 0 otherwise. |
| `beneficiary_spp` | Indicator for receipt of SPP benefits in the first cohort (1=yes, 0=no). |
| `spadies_any` | Indicator for immediate enrollment in any higher education institution (1=yes, 0=no). |
| `icfes_educm1` | Indicator that the mother's education is primary (1=yes, 0=no), used as a placebo outcome. |

The first five columns are complete. Mother's education is missing for 6,944
students. The scripts use separate complete-case samples for enrollment
(363,096 observations) and covariate balance (356,152 observations). No recoding
or imputation is performed. The simulation calibration uses the enrollment
sample. The SHA-256 checksum of `spp.csv` is:

```text
e8240222f3b2a4273ae0f9f93267f24b3312f46b8c6b544b22f925ea09d608ad
```

## Software Setup

The commands below use Bash on Linux. Run them from the replication folder.
Extract the bundled package once:

```sh
tar -xzf software/rd2d-4d8490e6.tar.gz -C software
```

| Implementation | Tested version | Bundled `rd2d` version |
| --- | --- | --- |
| Python | Python 3.14.4 | 1.0.0 |
| R | R 4.6.1 | 1.0.0 |
| Stata | Stata/MP 16, update 13 Jun 2023 | 1.0.1 |

The Python package requires Python 3.10 or newer, and the Stata package
requires Stata 16 or newer. External dependencies must be installed separately.

### Python

In a writable Python environment, install the bundled source with its plotting
dependencies:

```sh
python3 -m pip install './software/rd2d-4d8490e6/Python/rd2d[plots]'
```

The empirical scripts use NumPy, pandas, SciPy, and Matplotlib. Consult
`software/versions.txt` for the tested versions.

### R

Run the following in R. It creates a user library if necessary, installs the
external dependencies, and installs the bundled source:

```r
replication_library <- path.expand(Sys.getenv("R_LIBS_USER"))
dir.create(replication_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(replication_library, .libPaths()))
install.packages(c("MASS", "expm", "ggplot2"), repos = "https://cloud.r-project.org")
install.packages("software/rd2d-4d8490e6/R/rd2d", repos = NULL, type = "source")
```

Figures use `ggplot2` and were generated with the standard R PNG device (without
`ragg`). Installing `ragg` or using different system fonts may slightly change
image rendering without affecting the numerical results.

### Stata

From Stata, with the replication folder as the working directory:

```stata
net install rd2d, from("`c(pwd)'/software/rd2d-4d8490e6/stata") replace
which rd2d
```

A licensed Stata installation is required. The bundled package includes the
compiled Mata library (`.mlib`), so no Mata compilation is needed.

## Empirical Application

Run the analysis first, then the table and figure scripts for the same
language. The paper uses the ITT bandwidth selector (`bwitt`); Section SA-10.2
also reports fuzzy bandwidths (`bwmain`). All scripts compute both variants.

```sh
Rscript CTY_2027_JASA--empapp.R
Rscript CTY_2027_JASA--empapp_tables.R
Rscript CTY_2027_JASA--empapp_figures.R
```

In a separate copy of this folder, the Python commands are:

```sh
python3 CTY_2027_JASA--empapp.py
python3 CTY_2027_JASA--empapp_tables.py
python3 CTY_2027_JASA--empapp_figures.py
```

In another copy, set `STATA_EXE` to the licensed console launcher, then run:

```sh
export STATA_EXE=stata-mp
"$STATA_EXE" -b do CTY_2027_JASA--empapp.do
"$STATA_EXE" -b do CTY_2027_JASA--empapp_tables.do
"$STATA_EXE" -b do CTY_2027_JASA--empapp_figures.do
```

The R and Python scripts each generate 15 CSV files, 22 LaTeX table fragments,
and 46 figures. Point estimates, standard errors, and pointwise confidence
intervals agree up to floating-point tolerance, while simulated uniform
confidence bands and LBATE intervals differ slightly across random number
generators (both use 2,000 Gaussian draws, with seed `20260501` in R).

The Stata scripts generate 15 CSV files, 12 selected-point table fragments, and
13 figures covering point estimation, pointwise confidence intervals, and WBATE
inference (uniform confidence bands and LBATE intervals are not implemented in
Stata). To run Stata using the R or Python bandwidths, set
`RD2D_REFERENCE_OUTPUT` to the corresponding `output/` folder before running
`CTY_2027_JASA--empapp.do`.

### Figure and Table Map

Each output call identifies the manuscript number and panel, input CSV, and
output filename. R is the implementation used for the manuscript assets.

| Script | Manuscript outputs |
| --- | --- |
| `CTY_2027_JASA--empapp_figures.R` | Figures 1--2 and SA-1--SA-12, with separate calls for each panel; also 16 additional figure files. |
| `CTY_2027_JASA--empapp_tables.R` | Four panels of Table 1 and Tables SA-1--SA-12; also six additional table fragments. |
| `CTY_2027_JASA--empapp_figures.py`, `--empapp_tables.py` | Corresponding Python figures and tables, with the same output mapping. |
| `CTY_2027_JASA--empapp_figures.do`, `--empapp_tables.do` | The documented Stata subset described above. |
| `CTY_2027_JASA--simuls_tables.R` | Tables SA-13--SA-30 and six additional sharp/control tables. |

## Simulation Study

The R simulation script calibrates DGPs of degree `s=1,2,3` from the SPP data.
The ITT, first stage, and fuzzy analyses use the same simulated samples from
the fuzzy DGP, with both bandwidth selectors. Separate sharp DGPs produce
the six additional tables. All targets are evaluated on the 21-point grid;
WBATE and LBATE use the equal-weight average and maximum over this grid.

To reproduce the reported simulation tables, use the full defaults and set
the worker count to the CPUs available to the job. For example:

```sh
export OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1
RD2D_WORKERS=8 Rscript CTY_2027_JASA--simuls.R
Rscript CTY_2027_JASA--simuls_tables.R
```

| Variable | Default | Meaning |
| --- | --- | --- |
| `RD2D_M` | 5000 | Monte Carlo replications |
| `RD2D_N` | 10000 | Observations per replication |
| `RD2D_REPP` | 2000 | Gaussian draws for simulation inference |
| `RD2D_SEED` | 20260510 | Base seed; each replication/design/DGP determines its seed |
| `RD2D_WORKERS` | Available logical CPUs minus four, at least one and capped by `RD2D_M` | Parallel workers |
| `RD2D_OUTPUT_DIR` | `output` | Simulation CSV folder |
| `RD2D_TABLES_DIR` | `tables` | Simulation table folder |
| `RD2D_EMP_REPP` | 2000 | Gaussian draws for the R and Python empirical scripts |

The full simulation writes 45,000 replication CSV files plus five
metadata/calibration files and produces 24 LaTeX table fragments. On a 20-core
Linux server (40 logical CPUs, 122 GiB RAM), the simulation took about 28
minutes using 38 R workers. Because random seeds are assigned by replication
and design, the results do not depend on the number of workers.

The simulation uses joint local polynomial fits of degree 1 for point
estimation and degree 2 for robust bias correction with a
product triangular kernel, HC1 standard errors, unstandardized scores, no
mass-point adjustment, and a common scalar bandwidth at each boundary grid
point.

To run a quick test with two replications in separate output folders:

```sh
RD2D_M=2 RD2D_N=10000 RD2D_REPP=99 RD2D_WORKERS=2 \
  RD2D_OUTPUT_DIR=output-check Rscript CTY_2027_JASA--simuls.R
RD2D_OUTPUT_DIR=output-check RD2D_TABLES_DIR=tables-check \
  Rscript CTY_2027_JASA--simuls_tables.R
```

Note that `CTY_2027_JASA--simuls_tables.R` reads the replication count from the
saved run metadata and expects a complete set of replication files in
`RD2D_OUTPUT_DIR`.

## Output Files

- `output/empapp_*.csv`: empirical data, grid, metadata, and estimates.
- `output/simuls_raw_rep*.csv`: estimates and inference for each replication.
- Other `output/simuls_*.csv`: calibration parameters, analytic targets,
  estimand definitions, run settings, and runtime.
- `tables/empapp_*.tex`, `tables/simuls_*.tex`: LaTeX table fragments.
- `figures/*.png`: empirical figures.

The generated `.tex` files are table fragments intended for inclusion in the
manuscript rather than standalone LaTeX documents. Generated outputs in
`output/`, `tables/`, and `figures/` are not included in the replication
archive.

## Reference

- Cattaneo, M. D., R. Titiunik, and R. R. Yu (2027):
  [Estimation and Inference in Boundary Discontinuity Designs: Location-Based
  Methods](https://rdpackages.github.io/references/Cattaneo-Titiunik-Yu_2026_JASA.pdf).<br>
  _Journal of the American Statistical Association_, revise and resubmit.<br>
  [Supplemental Appendix](https://rdpackages.github.io/references/Cattaneo-Titiunik-Yu_2026_JASA--Supplement.pdf).

## Acknowledgment

This work was supported in part by the National Science Foundation through
grants [SES-2019432](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2019432),
[DMS-2210561](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2210561),
[SES-2241575](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2241575), and
[SES-2342226](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2342226), and by
the National Institute for Food and Agriculture through grant
[2024-67023-42704](https://www.nifa.usda.gov/data).
