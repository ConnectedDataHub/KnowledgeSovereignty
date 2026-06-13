# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

Replication materials for "Knowledge Sovereignty: A Path to Independence or Isolation?" — a quantitative study of how national citation self-preference interacts with international collaboration and research quality across countries.

## Repository layout

```
code/
  Data_09_22_2025.ipynb              # Python: constructs the panel dataset from raw sources
  Capactiy_07_30_2025.ipynb          # Python: capacity / field-size analysis
  Figures_*.ipynb                    # Python: main and SI figures
  SIFigures_*.ipynb                  # Python: supplementary figures
  regression/
    H1_03172026.R                    # R: Hypothesis 1 — self-preference → international collaboration
    H1_fields_03172026.R             # R: H1 replicated by scientific domain/field
    H2&3_03172026.R                  # R: Hypotheses 2 & 3 — quality outcomes
    H23_fields_03172026.R            # R: H2/3 replicated by domain
data/
  clean/                             # CSV inputs consumed by the R regression scripts
  raw/fields_data/                   # Compressed field hierarchy/info from OpenAlex
output/
  figures/                           # PDF figures written by R and Python scripts
  tables/                            # LaTeX table files written by R scripts
```

## Analysis pipeline

**Step 1 — Build the panel (Python, `Data_09_22_2025.ipynb`)**  
Reads raw AUC-based citation self-preference scores (from `CountryData2/`) and merges them with World Bank indicators, national authorship rates, topic diversity, and World Bank income classifications. Outputs a flat CSV to `RegressionSelfCitation/` (the old working directory, now at `data/clean/`).

**Step 2 — Regressions (R, `code/regression/`)**  
All four R scripts use `fixest::feols()` for two-way (Country + Year) fixed-effects panel regressions, clustered standard errors on Country, and `fixest::etable()` to emit `.tex` tables directly to the working directory. `marginaleffects::slopes()` computes marginal effects for the interaction plots.

**Step 3 — Figures (Python, `code/Figures_*.ipynb` and `code/SIFigures_*.ipynb`)**  
Matplotlib/Seaborn notebooks that produce trajectory plots, collaboration vs. self-citation comparisons, and capacity figures saved as PDFs under `output/figures/`.

## Running the code

### Python notebooks
```bash
jupyter notebook code/Data_09_22_2025.ipynb
```
Key libraries: `pandas`, `numpy`, `scipy`, `statsmodels`, `matplotlib`, `seaborn`.

### R scripts
```r
# From an R console or RScript — set the working directory first
setwd('/Users/psp2nq/Documents/KnowledgeSovereignty/code/regression')
source('H1_03172026.R')
```
Key packages: `fixest`, `marginaleffects`, `ggplot2`, `patchwork`, `ggtext`, `car`, `dplyr`.

To run field-level regressions, change `domain_id` (1–4 map to broad OpenAlex domains) in `H1_fields_03172026.R` and `H23_fields_03172026.R` before sourcing.

## Key variables

| Variable | Meaning |
|---|---|
| `logzscore` | Log-transformed AUC z-score — the core **citation self-preference** measure |
| `FracInternationalAuthors` | Share of authors from outside the focal country |
| `normalized_frac_top` | Country's top-journal publication share normalized by the world average |
| `hit_rate`, `novel_pct10_rate_norm`, `disrupt_top5_rate` | Alternative quality/impact outcomes |
| `income_group` | World Bank tier: `LM-L`, `UM`, `H` |
| `NResearchers` | Researchers per million (log₁₀-transformed before regression) |

## Data path note

The R scripts hardcode `setwd('/Users/psp2nq/Documents/NationalBiasOwn/RegressionSelfCitation')` and load CSVs from that path. The actual clean data now lives in `data/clean/`. Update `setwd()` and file paths when moving scripts or running on a different machine.
