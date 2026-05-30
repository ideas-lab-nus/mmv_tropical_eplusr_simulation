# MMV Tropical EnergyPlus/eplusr Simulation

This repository contains R notebooks and helper scripts for the tropical natural-ventilation EnergyPlus/eplusr analysis used in the manuscript figures.

## Research Context

This codebase is being prepared as supporting analysis code for Horikoshi et
al. (2026):

- Title: Mixed-mode ventilation potential coupled with air movement in tropical
  climates
- Authors: Kazuki Horikoshi, Federico Tartarini and Adrian Chong

## Main workflow

- `analysis/E+simulatior_multimetclo_daytime.Rmd`: upstream EnergyPlus/eplusr simulation workflow.
- `analysis/Analysis_multimetclo_daytime.Rmd`: analysis workflow for processed metrics and figures.
- `analysis/src/`: reusable helper functions used by the analysis notebook.
- `analysis/output/`: processed analysis data and generated figures included for inspection and figure regeneration.

## Included data

This public copy includes selected input/model data and processed analysis outputs:

- EPW data for the 20 paper cities under `analysis/data/epw/`.
- The original IDF input under `analysis/data/idf/1.original/`.
- The final DaytimeOnlyNV IDF set under `analysis/data/idf/11.setting_edit/250812_DaytimeOnlyNV/`.
- Processed analysis outputs under `analysis/output/data/`.
- Report figures under `analysis/output/figures/`.

The final simulations use the DaytimeOnlyNV IDF set. The natural-ventilation lower outdoor-temperature condition is fixed at 20 deg C in the simulation outputs used by this analysis.

Full recomputation from raw simulation outputs requires the raw PMV and EnergyPlus snapshot folders listed above.

## Environment

The recommended development environment is the devcontainer in `.devcontainer/devcontainer.json`, which uses:

`hkazukinus/ideaslab_env_eplusr:2025nov`

The notebooks assume R, eplusr, EnergyPlus 9.5.0, and the R packages loaded by `analysis/config.R`.

## Running the analysis notebook

Open `analysis/Analysis_multimetclo_daytime.Rmd` from the `analysis/` directory. The notebook initializes paths with `config.R` and writes analysis-generated outputs under `analysis/output/`.

For figure inspection without full recomputation, use the processed data already included under `analysis/output/data/`.
