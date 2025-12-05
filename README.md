# MCED Spillover Paper

### Project Description

This repository contains code that reproduces the analyses in our fourthcoming MCED spillover paper, which studies how multi-cancer early detection pilots affect diagnostic delays within NHS England. The R code covers data acquisition and cleaning, panel dataset construction, propensity-score weighting for sensitivity checks, and simulation work that informs inference around selection bias.

### Repository Structure

```
R/                        # R scripts split by workflow
  analysis/               # Panel analysis (e.g., mced_panel_analysis.R)
  data_cleaning/          # Data ingestion and preprocessing scripts
  psw/                    # Propensity-score weighting workflow
  simulations/            # Simulation scripts and helpers (under development)
data/                     # Data folders (non-sensitive only)
  cleaned_data/
  linking_files/
  processed/
  raw_covariates/
  results/
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
* The code should be run in the following order:

1. `R/data_cleaning/mced_panel_datacollection.R`
2. `R/psw/PSW_work.R`
3. `R/analysis/mced_panel_analysis.R`

All datasets needed to run these scripts are contained in the repo.

### Contributors

[Sean Mann](smann@rand.org), [Pedro Nascimento de Lima](plima@rand.org), [Beth Ann Griffin](bethg@rand.org), [Joshua Eagan](jeagan@rand.org)

For questions or suggestions, please open an issue or contact the project maintainer.

### Data

Data included in this repository were obtained from `https://www.england.nhs.uk/`, see code comments for specific URLs. These data are made available for public use under the [OGL 3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/ "license text") license.

### License

This code is Copyright (C) RAND 2025 and is licensed under version 3 of the GPL or any later version.
