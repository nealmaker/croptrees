# Crop Tree Decision Rules - Supplemental Data

This directory contains decision tree models and simplified rules for identifying timber crop trees in the northeastern United States. The analysis considers multiple species, site types, and discount rates.

## Overview

Crop trees are trees that should be retained and given growing space because their expected future value growth exceeds opportunity costs. This analysis uses simulation-based net present value (NPV) calculations to identify optimal crop tree criteria based on:

- **DBH** (diameter at breast height, inches)
- **Crown ratio** (CR, percent of tree height with live crown)
- **Best log grade** (1 = veneer quality, 2 = sawlog quality)

## Species Codes

| Code | Common Name | Scientific Name |
|------|-------------|-----------------|
| BM | Sugar Maple | *Acer saccharum* |
| PB | Paper Birch | *Betula papyrifera* |
| RM | Red Maple | *Acer rubrum* |
| SO | Red Oak | *Quercus rubra* |
| WO | White Oak | *Quercus alba* |
| WP | White Pine | *Pinus strobus* |
| YB | Yellow Birch | *Betula alleghaniensis* |

**Note:** White Pine (WP) is never a crop tree under the economic assumptions used in this analysis.

## Site Types

| Site | Description |
|------|-------------|
| clayplain | Champlain Valley clayplain forests |
| granitictill | Granitic till upland forests |
| mountaintill | Northern hardwood mountain forests |
| richnhw | Rich northern hardwood sites |

## Discount Rates

Models were fit for discount rates of 2%, 3%, 4%, 5%, and 6%.

## File Descriptions

### Decision Tree Files

- **`dt_[SPP]_[SITE]_[RATE]pct.rds`** - R `rpart` decision tree objects
  - Example: `dt_BM_clayplain_3pct.rds` = Sugar Maple on clayplain at 3% discount rate
  - Load with: `tree <- readRDS("dt_BM_clayplain_3pct.rds")`

- **`dt_[SPP]_[SITE]_[RATE]pct.png`** - Visual representation of crop tree rules extracted from each decision tree

### Summary Files

- **`crops_all.rds`** - Training data used to fit decision trees (116,280 observations)
  - Columns: `spp`, `site`, `drate`, `dbh`, `cr`, `best_log`, `crop` (TRUE/FALSE)

- **`simplified_rules.csv`** - Simplified threshold values extracted from decision trees

- **`simplified_rule_accuracy.csv`** - Accuracy metrics comparing simplified rules to original trees

- **`rule_summaries.txt`** - Human-readable prose summaries of crop tree rules by species

- **`summary_report.rds`** - Complete R object with all analysis results

## Simplified Rules for Field Use

The following rules approximate the decision tree models for practical field application at a **3% discount rate**. Adjust DBH thresholds as noted for different discount rates and sites.

### Sugar Maple (BM)
- **Base rule:** Crop tree if DBH ≤ 13" for sawtimber, or ≤ 14" if veneer-quality
- **Requires:** Crown ratio ≥ 35%
- **Discount rate adjustment:** -0.8" per 1% increase from 3%
- **Site adjustment:** granitictill -0.8"

### Paper Birch (PB)
- **Base rule:** Crop tree only if veneer-quality AND DBH ≤ 16"
- **Requires:** Crown ratio ≥ 45%
- **Note:** Sawlog-quality paper birch are not crop trees
- **Discount rate adjustment:** -0.7" per 1% increase from 3%
- **Site adjustments:** mountaintill +1.2", clayplain -0.8", richnhw -0.8"

### Red Maple (RM)
- **Base rule:** Crop tree if veneer-quality and DBH ≤ 17", or sawlog-quality and DBH ≤ 14"
- **Requires:** Crown ratio ≥ 25%
- **Discount rate adjustment:** -1.0" per 1% increase from 3%
- **Site adjustments:** granitictill +0.8", mountaintill +0.8", clayplain -1.2"

### Scarlet Oak (SO)
- **Base rule:**
  - Crown ratio ≥ 35%: DBH ≤ 18"
  - Crown ratio < 35%: DBH ≤ 13"
  - Veneer-quality trees can be +4" larger
- **Discount rate adjustment:** -1.1" per 1% increase from 3%
- **Site adjustments:** mountaintill +2.2", granitictill -0.8", clayplain -1.8"

### White Oak (WO)
- **Base rule:**
  - Crown ratio ≥ 35%: DBH ≤ 19"
  - Crown ratio < 35%: DBH ≤ 13"
  - Veneer-quality trees can be +5" larger
- **Discount rate adjustment:** -1.4" per 1% increase from 3%
- **Site adjustments:** mountaintill +1.8", clayplain -1.2"

### Yellow Birch (YB)
- **Base rule:**
  - Crown ratio ≥ 30%: DBH ≤ 21"
  - Crown ratio < 30%: DBH ≤ 17"
- **Note:** Trees with better crowns can be retained at larger sizes
- **Discount rate adjustment:** -1.6" per 1% increase from 3%
- **Site adjustments:** richnhw +1.5", granitictill -1.5"

### White Pine (WP)
- **Never a crop tree** under these economic assumptions (harvest immediately)

## Accuracy Summary

Performance of simplified rules vs. original decision trees (all discount rates):

| Species | Simplified Accuracy | Tree Accuracy | Simplified F1 | Tree F1 |
|---------|--------------------:|-------------:|-------------:|--------:|
| BM | 95.2% | 96.3% | 0.802 | 0.860 |
| PB | 94.5% | 96.0% | 0.661 | 0.688 |
| RM | 90.5% | 93.8% | 0.776 | 0.829 |
| SO | 77.7% | 94.4% | 0.661 | 0.872 |
| WO | 79.4% | 93.7% | 0.747 | 0.897 |
| YB | 91.9% | 94.0% | 0.911 | 0.931 |

**Interpretation:**
- The original decision trees prioritize **precision** (rarely marking non-crop trees as crop)
- The simplified rules trade some precision for higher **recall** (catching more actual crop trees)
- For SO and WO, the simplified rules are notably more liberal than the original trees

## Usage in R

```r
# Load a decision tree
tree <- readRDS("dt_BM_clayplain_3pct.rds")

# Make predictions for new data
new_data <- data.frame(dbh = 12, cr = 40, best_log = 1)
predict(tree, new_data, type = "class")

# Load training data
crops_all <- readRDS("crops_all.rds")

# Load full summary report
report <- readRDS("summary_report.rds")
```

## Citation

[Add citation information when published]

## Contact

[Add contact information]
