## Overview

This repository contains the reproducibility materials for the study ***Collective Behavior under Heterogeneous Information Exposure***.

## Repository structure

- `scripts/` — R scripts
- `results/` — analysis results
- `figures/` — generated figures
- `renv/` — project environment infrastructure
- `renv.lock` — R package versions
- `.Rprofile` — activates the project environment

## Reproducing the analysis

1. Restore the R environment with `renv::restore()`.
2. Run the scripts in `scripts/` in numerical order.