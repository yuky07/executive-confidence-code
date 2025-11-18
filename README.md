# Executive Confidence and Firm Performance: Pandemic Impacts, Quantile Effects, and Organizational Dynamics— R Code 

This repository contains the R code used in the analysis for the paper *“Executive Confidence and Firm Performance: Pandemic Impacts, Quantile Effects, and Organizational Dynamics”*

## Contents

- `00_setup.R`  
  Loads packages and defines helper functions for covariance estimation for linear and quantile regression models.

- `01_main_regressions.R`  
  Two-way fixed-effects regressions (firm and year) of Tobin's Q, ROA, and ROE on the structural confidence proxy and control variables.

- `02_covid_dummy_regressions.R`  
  Fixed-effects regressions with COVID-19 year dummies (2020, 2021) and interaction terms between executive confidence and COVID-period indicators.

- `03_quantile_regressions.R`  
  Fixed-effects quantile regressions at τ = 0.10, 0.25, 0.50, 0.75, and 0.90 for Tobin's Q, ROA, and ROE, including firm and year effects.

- `04_robustness_checks.R`  
  Robustness and endogeneity tests, including:
  - Industry × year fixed effects (adding industry-by-year dummies);
  - Alternative size proxy (log sales) in place of log assets;
  - Placebo regressions with lead confidence (t+1);
  - Interaction models for confidence × Gen and confidence × FOP;
  - Interaction models for confidence × Closeheld (ownership concentration).

## How to Use the Code

1. Create the dataset  
   `data/executive_confidence_panel.rds`  
   following the variable definitions and sample construction described in the paper.

   The dataset should at minimum include:
   - `firm_id`, `year`, `Industry`
   - `TobinQ`, `ROA`, `ROE`
   - `Confidence`, `Size`, `Lev`, `SalesGrowth`, `Age`, `Gen`, `FOP`
   - `Size_sales` (log sales, for the alternative size robustness)
   - `Closeheld` (for the ownership concentration interaction tests)

2. Place `executive_confidence_panel.rds` in the `data/` folder (or adjust the path inside the scripts).

3. Run the scripts in order if reproducing the full analysis:
   - `01_main_regressions.R`
   - `02_covid_dummy_regressions.R`
   - `03_quantile_regressions.R`
   - `04_robustness_checks.R`

All output tables will be saved automatically in the `output/tables/` directory.

## Data Availability

The firm-level dataset used in the paper is not included in this repository.  
It is available from the corresponding author upon reasonable request.

## Code Availability

This repository contains the complete R code used for:
- main fixed-effects regressions,
- COVID-period interaction models,
- quantile regressions across multiple quantiles with fixed effects, and
- robustness and endogeneity checks (industry×year FE, alternative size, lag/lead confidence, and interaction models).

The code is provided to support transparency and reproducibility.
