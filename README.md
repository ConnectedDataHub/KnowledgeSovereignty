# Project overview
Replication materials for Knowledge Sovereignty: A Path to Independence or Isolation?,  including data, code, and documentation for reproducing the analyses and figures presented in the paper.


## Repository layout

```
code/
  panel_data.ipynb                    # Python: constructs the main panel dataset from raw sources
  panel_data_fields.ipynb             # Python: constructs the field-level panel dataset
  main_table1&figure2.ipynb           # Python: Table 1 (descriptive stats) and Figure 2
  main_figure5_trajectroies.ipynb     # Python: Figure 5 (country trajectory plots)
  SI_figures.ipynb                    # Python: supplementary figures S1–S5
  SI_compare_capactiy_index.ipynb     # Python: capacity index validation (Figures S11, Tables S9–S12)
  regression/
    H1.R                              # R: Hypothesis 1 — self-preference → international collaboration
    H1_fields.R                       # R: H1 replicated by scientific domain/field
    H2&3.R                            # R: Hypotheses 2 & 3 — quality outcomes
    H23_fields.R                      # R: H2/3 replicated by domain
data/
  clean/                              # Processed CSVs consumed by R regression scripts
  raw/
    CountryData/                      # Raw citation, authorship, World Bank, and auxiliary data
    fields_data/                      # Compressed field hierarchy/info from OpenAlex
    *.csv.gz                          # Intermediate novelty, disruption, and hit-rate files
output/
  figures/                            # PDF figures written by R and Python scripts
  tables/                             # LaTeX table files written by R scripts
```

## Data availability

The following large raw data files are **not included in this repository** due to GitHub's file size limit, but are required to re-run the pipeline from scratch:

| File | Size | Used by |
|---|---|---|
| `data/raw/CountryData/` | ~1.2 GB | All notebooks — contains raw citation AUC files, World Bank indicators, authorship rates, income classifications, and auxiliary country data |
| `data/raw/oa_countrycites_noselfauthor.csv.gz` | ~850 MB | `main_table1&figure2.ipynb` — raw country-level citation matrix |
| `data/raw/oa_countrycites_noselfauthor_aucboot.csv.gz` | ~183 MB | `panel_data.ipynb` — bootstrap AUC scores (main sample) |
| `data/raw/oa_countrycites_noselfauthoraff_aucboot.csv.gz` | ~177 MB | `panel_data.ipynb` — bootstrap AUC scores (affiliation-based sample) |

All processed regression-ready datasets (in `data/clean/`) and intermediate files (in `data/raw/*.csv.gz`) **are included** in this repository and are sufficient to reproduce all regression tables, figures, and supplementary analyses without reconstructing the panel datasets.