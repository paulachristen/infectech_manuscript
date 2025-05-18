# Bridging the Gap: Enhancing the Evaluation &amp; Interpretation of Epidemic Forecasts for Researchers &amp; Policymakers in Resource-Constrained Settings.
Code and data for the manuscript "Bridging the Gap: Enhancing the Evaluation &amp; Interpretation of Epidemic Forecasts for Researchers &amp; Policymakers in Resource-Constrained Settings." This repository contains all analysis scripts, plotting code, and data processing steps used in the study, supporting reproducibility and transparency.


## 📂 Repository Structure

```
.
├── analysis.Rmd             # Main analysis script (data cleaning, descriptive stats)
├── plots.Rmd                # Script for generating publication-ready figures
├── code/
│   └── functions.R          # Support functions used across the analysis and plotting scripts
├── data/
│   ├── raw/                 # Raw survey response files (CSV)
│   └── countries_regions.xlsx  # Country-to-region mapping
├── output/
│   ├── figures/             # Final figures for manuscript
│   └── tables/              # Summary tables, if applicable
```

## 📊 Analysis Workflow

1. **Data Loading and Cleaning**
   - All raw CSV survey responses are loaded and combined.
   - Responses are cleaned for missing values, placeholders (e.g. `"Other. Please specify:"`), and merged with a country-to-region dictionary.

2. **Section Completion Flags**
   - Binary indicators are generated for whether respondents completed key survey sections (e.g., Metrics, Questions, Evaluation).
   - These are used to define denominators in descriptive statistics and plotting.

3. **Visualizations**
   - Plots are generated using `ggplot2` with consistent themes and color palettes.
   - Section-specific figures are faceted by income group and exported as publication-quality graphics.

4. **Support Functions**
   - All reusable components (e.g., group label formatting, palette assignment) are housed in `functions.R`.

## 🛠 Requirements

Install the following R packages before running the scripts:

```r
install.packages(c(
  "tidyverse", "here", "readxl", "stringr", "gt",
  "openxlsx", "survival", "survminer", "psych",
  "viridis", "ggtext", "gridExtra", "RColorBrewer", "tidytext"
))
```

## 📘 Citation

If you use or build upon this codebase, please cite the associated paper:

> Christen, P., et al. *Bridging the Gap: Enhancing the Evaluation & Interpretation of Epidemic Forecasts for Researchers & Policymakers in Resource-Constrained Settings.* The Lancet Digital Health (in review), 2025.

## ✉️ Contact

For questions or feedback, please contact [Paula Christen](mailto:paula.christen@cema.africa).
