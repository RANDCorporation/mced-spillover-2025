# MCED Spillover Paper

### Project Description

This repository contains code that reproduces the analyses in our fourthcoming MCED spillover paper, which studies how multi-cancer early detection pilots affect diagnostic delays within NHS England. The R code covers data acquisition and cleaning, panel dataset construction, propensity-score weighting for sensitivity checks, and simulation work that informs inference around selection bias.

### Repository Structure

```
R/                        # R scripts split by workflow
  analysis/               # Panel analysis and figures
  data_cleaning/          # Data ingestion and preprocessing scripts
  psw/                    # Propensity-score weighting workflow
data/                     # Data folders (non-sensitive only)
  cleaned_data/           # Outputs from data_cleaning and psw scripts
  linking_files/          # Geographic and hierarchical mapping files
  raw_covariates/         # Raw data files from external sources
  results/                # Analysis outputs (figures, tables, CSVs)
renv/                     # renv environment
renv.lock                 # Locked package versions
mced-spillover.Rproj      # RStudio project file
```

### Reproducibility with `renv`

The project uses [renv](https://rstudio.github.io/renv/) for R package management. To set up your environment, open the R project in an IDE that supports R projects like RStudio or Positron. Then, run:

```{R}
renv::status() # checks what changes if any are needed to get the project into sync
renv::restore() # Installs required R packages 
```

When adding new packages:

```{R}
renv::snapshot()  # Updates package metadata 
```

### Instructions

To reproduce our analysis:

* Follow the steps above to set up your R packages using renv, this must be done from inside the R project in your IDE.
* The scripts should be run in the four stages below. Scripts within the same stage can be run in any order; each stage must complete before starting the next.

All datasets needed to run these scripts are contained in the repo.

#### Stage 1 — Data Collection & Cleaning

These scripts build the panel datasets and Cancer Alliance-level covariates from raw data files included in the repo.

1. `R/data_cleaning/mced_panel_datacollection.R` — Constructs the main 28-day Faster Diagnosis Standard (FDS) panel dataset from monthly NHS England files. Outputs `data/cleaned_data/base_data.csv`.
2. `R/data_cleaning/tww_datacollection.R` — Constructs the Two Week Wait (TWW) panel dataset. Outputs `data/cleaned_data/tww_base_data.csv`.
3. `R/data_cleaning/stage_processing.R` — Processes cancer stage at diagnosis data by Cancer Alliance. Outputs `data/cleaned_data/stage_by_cancer_alliance.csv`.
4. `R/data_cleaning/imd_processing.R` — Computes population-weighted Index of Multiple Deprivation (IMD) measures by Cancer Alliance. Outputs `data/cleaned_data/imd_by_cancer_alliance.csv`.
5. `R/data_cleaning/ethnicity_processing.R` — Computes ethnic composition by Cancer Alliance from Census 2021 data. Outputs `data/cleaned_data/ethnicity_by_cancer_alliance.csv`.
6. `R/data_cleaning/mortality_processing.R` — Computes population-weighted age-standardised cancer mortality rates by Cancer Alliance. Outputs `data/cleaned_data/mortality_by_cancer_alliance.csv`.

#### Stage 2 — Propensity-Score Weights for Sensitivity-Analysis

7. `R/psw/PSW_work.R` — Fits the a propensity-score model (staff numbers and population) and computes ATT weights. Outputs `data/cleaned_data/PS_wts_for_sensruns.csv`.
8. `R/psw/PSW_stage_work.R` — PSW accounting for cancer stage imbalance. Outputs `data/cleaned_data/PS_wts_stage_for_sensruns.csv`.
9. `R/psw/PSW_IMD_work.R` — PSW accounting for area deprivation imbalance. Outputs `data/cleaned_data/PS_wts_IMD_for_sensruns.csv`.
10. `R/psw/PSW_ethnicity_work.R` — PSW accounting for ethnic composition imbalance. Outputs `data/cleaned_data/PS_wts_ethnicity_for_sensruns.csv`.
11. `R/psw/PSW_mortality_work.R` — PSW accounting for cancer mortality rate imbalance. Outputs `data/cleaned_data/PS_wts_mortality_for_sensruns.csv`.

#### Stage 3 — Analysis

12. `R/analysis/mced_panel_analysis.R` — Main difference-in-differences analysis of FDS outcomes (diagnostic delay rate, referral rate, estimated wait time) with all sensitivity analyses. Outputs result tables and figures to `data/results/`.
13. `R/analysis/tww_analysis.R` — Difference-in-differences analysis of TWW outcomes (breach rate, referral rate, estimated wait time). Outputs result tables to `data/results/`.
14. `R/analysis/forest_plots_estWaitTime.R` — Generates forest plot figures for estimated wait time outcomes. Outputs EPS figures to `data/results/`.

### Contributors

[Sean Mann](smann@rand.org), [Pedro Nascimento de Lima](plima@rand.org), [Beth Ann Griffin](bethg@rand.org), [Joshua Eagan](jeagan@rand.org)

For questions or suggestions, please open an issue or contact the project maintainer.

### Data

Data included in this repository were obtained from `https://www.england.nhs.uk/`, see code comments for specific URLs. These data are made available for public use under the [OGL 3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/ "license text") license.

### License

This code is Copyright (C) RAND 2025 and is licensed under version 3 of the GPL or any later version.
