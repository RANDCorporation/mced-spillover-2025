### Workflow & Collaboration

Primary workflows live under `R/`: start with `data_cleaning/mced_panel_datacollection.R` to assemble cleaned inputs, follow with `analysis/mced_panel_analysis.R` for core estimates, use `psw/PSW_work.R` when generating PS weights and balance checks, and rely on `simulations/` for selection-bias simulations (under development).

### Getting Started

Fork the repository from [RANDCorporation on GitHub](https://github.com/RANDCorporation) and clone your fork locally:

```
git clone git@github.com:<your-username>/mced-spillover.git
cd mced-spillover
```

Add the upstream remote (the official RANDCorporation repository) so you can pull in updates:

```
git remote add upstream git@github.com:RANDCorporation/mced-spillover.git
git fetch upstream
git switch dev
```

At the start of each session, make sure you are up to date with the upstream `dev` branch:

```
git fetch upstream
git switch dev
git merge upstream/dev  # or use rebase if you prefer
```

After making changes, commit to a feature branch that lives on your fork and push to your fork (`origin`):

```
git switch dev
git switch -c <feature-branch>
# ... make edits ...
git add <files>
git commit -m "Describe your changes"
git push -u origin <feature-branch>
```

### Branching

Work off the latest `upstream/dev` and create feature branches (e.g., `username/feature-description`) for all changes. Push feature branches to your fork (`origin`). Merges to `main` are managed after RAND QA review.

### Pull Requests

- Open pull requests (PRs) from your fork's feature branch into `upstream/dev`.
- Summarize the key changes, reference related issues, and note any data or configuration updates.
- Tag at least one reviewer and respond to feedback promptly.

### Folder Guidelines

`R/`: Place R scripts in the appropriate workflow sub-folder. `data/`: Store public or shareable project data only. `data/results`: Save generated tables, figures, and serialized model objects.

Before opening a PR:

- Run the workflow scripts and confirm outputs are up to date.
- Use `renv::status()` to ensure your library is in sync and `renv::snapshot()` only when new dependencies are intentional.
- Remove local scratch files or large intermediate data not meant for version control.
