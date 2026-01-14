# croptrees

Developing criteria for choosing timber crop trees in the US Northeast.

This project uses simulation-based net present value (NPV) analysis to identify which trees should be retained during a logging entry and actively favored ("crop trees"). It assumes that any retained crop trees would remain free-to-grow until thier eventual harvest. As such, it does not account for competition between retained trees; if an area is well-stocked with crop-tree quality trees, it may be beneficial to remove some of them to favor others. Decision rules are derived from decision trees fit to simulation outcomes.

## Quick Start

```r
# Run the full analysis pipeline
source("R/run.R")

# Or generate simplified rules from existing decision trees
source("R/summarize-decision-trees.R")
summarize_crop_tree_rules("output/decision-trees/dataset2")
```

## Supplemental Data

The `output/decision-trees/dataset2/` directory contains supplemental data including:

- Decision tree models (.rds files) for each species/site/discount rate combination
- Visual rule diagrams (.png files)
- Simplified field rules with accuracy metrics
- Complete documentation in README.md

See `output/decision-trees/dataset2/README.md` for detailed documentation.

## Key Findings

| Species | Base Rule (3% discount rate) | Notes |
|---------|------------------------------|-------|
| Sugar Maple | DBH ≤ 13" (14" if veneer), CR ≥ 35% | Adjust -0.8"/1% discount rate |
| Paper Birch | Veneer only, DBH ≤ 16", CR ≥ 45% | Sawlog never crop tree |
| Red Maple | Veneer ≤ 17", Sawlog ≤ 14", CR ≥ 25% | Site-dependent |
| Scarlet Oak | CR ≥ 35%: ≤ 18"; CR < 35%: ≤ 13" | +4" for veneer |
| White Oak | CR ≥ 35%: ≤ 19"; CR < 35%: ≤ 13" | +5" for veneer |
| Yellow Birch | CR ≥ 30%: ≤ 21"; CR < 30%: ≤ 17" | Largest DBH thresholds |
| White Pine | Never a crop tree | Harvest immediately |

## Scripts

- **R/data.R** - Builds the starting dataset of trees to consider (with more species and sites, and at a coarser scale), defines parameters to guide simulations, and creates custom functions.

- **R/simulation.R** - Sources data.R, runs a simulation for each tree in the dataset, and creates a 'sim' object with trees' attributes and values at each timestep.

- **R/lev.R** - Creates a data frame of sites and discount rates with associated LEVs (Land Expectation Values, which account for the value of future regeneration), based on expected replacement trees and NPV calculations. Saves devomposed LEV calculations (weighted LEVs of individual trees for each site / discount rate combination) to output/replacement-tree-lev.

- **R/fct-croptrees.R** - Defines a function that takes simulation data, a discount rate, a price factor, and the LEV table, and returns a data frame identifying crop trees.

- **R/coarse-analysis.R** - Builds a Random Forest model predicting crop trees based on fct_croptrees output, with ML interpretation to: 1) determine the most important crop tree predictors, 2) determine the optimal arrangement of decision trees to train, and 3) perform exploratory data analysis. Saves visualizations to output/base-rate-rf-analysis, sorted by hardwood (HW) and softwood (SW) species. 

- **R/grades-analysis.R** - Extends coarse analysis by exploring relationships between grade and value growth, absent LEV. Saves plots to output/base-rate-rf-analysis, sorted by HW and SW. Further sorted into values-by-spp-and-grade (comparisons between trees of different grades) and bolt-analysis (breaking a tree into its component log bolts) directories.

- **R/fct-decision-tree.R** - Functions for fitting and visualizing decision trees from crop tree data.

- **R/summarize-decision-trees.R** - Extracts and summarizes decision tree rules into concise field-usable guidelines with discount rate and site adjustments.

- **R/evaluate-simplified-rules.R** - Calculates accuracy metrics (precision, recall, F1, FPR, FNR) comparing simplified rules to original decision trees.

- **R/run.R** - Sources all scripts in order to run the entire analysis.

## Output Structure For Final Rules

```
output/
├── decision-trees/
│   └── dataset2/           # Publication-ready supplemental data
│       ├── README.md       # Detailed documentation
│       ├── crops_all.rds   # Training data (116,280 obs)
│       ├── dt_*.rds        # Decision tree models
│       ├── dt_*.png        # Rule visualizations
│       ├── simplified_rules.csv
│       ├── simplified_rule_accuracy.csv
│       └── rule_summaries.txt
└── ...
```

## Requirements

- R (≥ 4.0)
- Required packages: rpart, dplyr, tidyr, ggplot2, renv

## Citation

[Add citation information when published]

## Contact

Neal Maker
neal@forestbiometrics.org
