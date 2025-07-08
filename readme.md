
# MCED Spillover Paper

### 📄 Project Description

Add your description here. Briefly describe the purpose of the MCED Spillover Paper, its main research questions, and what the repository contains.

### 📁 Repository Structure

``` 
├── R/                # R scripts and code (create subfolders as needed)
├── data/             # Project data (public only)
├── output/           # Outputs: tables, models, graphs, etc.
├── archive/          # Extraneous files not needed for reproducibility
├── renv/             # renv environment for reproducibility
├── renv.lock         # renv lockfile (R package versions)
├── mced-spillover.Rproj # RStudio project file
├── .gitignore        # Files and folders to ignore in git
├── .Rprofile         # R project profile settings
├── .git/             # Git version control folder
├── .Rproj.user/      # RStudio project user settings
└── README.md         # This file 
```

### 🚦 Workflow & Collaboration

### Getting Started

<br>Clone the repository:</br> 
``` 
git pull git@code.rand.org:mced-spillover/mced-spillover.git 
git switch dev 
```
<br>At the start of each session:</br> ``` git pull ```
<br>After making changes:</br>
``` 
git add . 
git commit -m "your description of the changes"
git push -u origin dev 
```

### Branching:

Work in the ```dev``` branch for all non-QAed changes.
Merges to ```main``` will be managed after QA.

###  📦 Reproducibility with ```renv```
The project uses <a href="https://rstudio.github.io/renv/">renv</a> for R package management.
To set up your environment: 

```
renv::restore() # Installs required R packages 
```

When adding new packages: 

``` 
renv::snapshot()  # Updates package metadata 
```

### 🗂️ Folder Guidelines

```R/```: Place all R scripts here. Create descriptive subfolders as needed.

```data/```: Store public project data here.

```output/```: Save outputs (tables, models, graphs, etc.) here.

```archive/```: Place any non-essential files here.

<br>No sensitive data or API keys:</br> Only commit public, shareable content.

### 📝 Notes
This repository is intended to be public at project completion.
Please ensure all committed files are suitable for public release.

### 👥 Contributors
[Your Name(s) Here]
For questions or suggestions, please open an issue or contact the project maintainer.
Let me know if you want further customization or additional sections!
