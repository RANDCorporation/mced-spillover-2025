# MCED Spillover Paper

### Project Description

This repository supports the MCED spillover paper, which studies how multi-cancer early detection pilots affect diagnostic delays within NHS England. The R code covers data acquisition and cleaning, panel dataset construction, propensity-score weighting for sensitivity checks, and simulation work that informs inference around selection bias.

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
.gitignore
.Rprofile
readme.md
```

### Workflow & Collaboration

Primary workflows live under `R/`: start with `data_cleaning/mced_panel_datacollection.R` to assemble cleaned inputs, follow with `analysis/mced_panel_analysis.R` for core estimates, use `psw/PSW_work.R` when generating PS weights and balance checks, and rely on `simulations/` for selection-bias simulations (under development).

### Getting Started

Clone the repository:

```
git clone git@code.rand.org:mced-spillover/mced-spillover.git
cd mced-spillover
git switch dev
```

At the start of each session: `git pull` After making changes:

```
git add .
git commit -m "describe your changes"
git push -u origin dev
```

### Branching

Work in the `dev` branch for all non-QAed changes. Merges to `main` will be managed after QA review.

### Reproducibility with `renv`

The project uses [renv](https://rstudio.github.io/renv/) for R package management. To set up your environment, open the R project in r studio or positron. Then, run:

```{R}
renv::status() # checks what changes if any are needed to get the project into sync
renv::restore() # Installs required R packages 
```

When adding new packages:

```{R}
renv::snapshot()  # Updates package metadata 
```

The code should be run in the following order:

1. `R/data_cleaning/mced_panel_datacollection.R`
2. `R/psw/PSW_work.R`
3. `R/analysis/mced_panel_analysis.R`

All datasets needed to run these scripts are contained in the repo.

### Folder Guidelines

`R/`: Place R scripts in the appropriate workflow sub-folder. `data/`: Store public or shareable project data only. `data/results`: Save generated tables, figures, and serialized model objects.

### Contributors

Sean Man, Pedro Nascimento de Lima, Beth Ann Griffin, Joshua Eagan

For questions or suggestions, please open an issue or contact the project maintainer.

### Data

Data included in this repository were obtained from `https://www.england.nhs.uk/`, see code comments for specific URLs. These data are made available for public use under the [OGL 3](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/ "license text") license.

### License

This code is licensed under version 3 of the GPL or any later version.
