# Crop Tree Decision Rules for the Northeastern United States

## Project Overview

This project develops quantitative criteria for identifying timber crop trees in the northeastern United States. A crop tree is a tree that should be retained and given growing space during a harvest entry because its expected future value growth exceeds the opportunity cost of the capital tied up in the tree and the land it occupies.

### Goals

1. Develop simulation-based methods to identify optimal crop tree retention decisions
2. Identify the key tree and site attributes that drive crop tree classification
3. Create simplified, field-usable decision rules for common species and site types
4. Quantify the accuracy and sensitivity of these rules to economic assumptions

---

## Methods

### Overview of Analytical Pipeline

The analysis proceeds through several stages:

1. **Data Generation**: Create a comprehensive, orthogonal dataset of hypothetical trees varying in species, size, crown ratio, timber quality, and site
2. **Growth Simulation**: Project each tree's value over time if retained and fully released, using growth, mortality, and timber volume and value models
3. **LEV Calculation**: Compute site- and discount rate-specific Land Expectation Values representing the opportunity cost of delaying harvest of a single tree
4. **Crop Tree Classification**: Identify which trees are worth retaining and growing for at least 15 more years, based on net present value analysis
5. **Coarse Analysis**: Train a Random Forest model to identify important crop tree predictors and guide decision tree structure
6. **Decision Tree Fitting**: Fit interpretable decision trees to identify crop trees for each species/site/discount rate combination (as determined through coarse analysis)
7. **Rule Simplification**: Extract simplified prose crop tree rules suitable for field application
8. **Accuracy Evaluation**: Compare simplified rules to original decision trees and to original crop tree classification
9. **Sensitivity Analysis**: Test how rules respond to changes in the likelihood of mortality, stumpage prices, and LEV

### Data Generation

#### Initial Dataset (data.R)

The initial exploratory dataset includes a broad range of species and sites:

**Species** (11 total):
- Hardwoods: Black Cherry (BC), Hard Maple (BM), Paper Birch (PB), Red Maple (RM), Red Oak (SO), White Oak (WO), Yellow Birch (YB)
- Softwoods: Balsam Fir (BF), Eastern Hemlock (EH), Red Spruce (RS), White Pine (WP)

**Sites** (5 total):
- `granitictill`: Hardwood-white pine site on Becket sandy loam soils at Paul Smith's College Forest in Paul Smith's, NY (site class 5, elev 1300')
- `richnhw`: Rich northern hardwood site on deep loamy Salmon-Adamant complex soils along the Winooski River in Moretown, VT (site class 4, elev 700')
- `mountaintill`: Beech-red maple site on convex mountain slope in Bartlett NH, Monadnock granitic till soils (site class 5, elev 1500')
- `lowlandspfr`: Lowland spruce-fir site on poorly-drained Brayton lodgment till soil in Dallas, ME (site class 6, elev 1700') [excluded from final analysis]
- `clayplain`: Mesic clayplain site on Vergennes clay at Williams Woods Natural Area in Charlotte, VT (site class 5, elev 120')

**Tree attributes varied**:
- DBH: 4-26 inches (3-inch increments initially)
- Crown ratio: 20-70% (10% increments)
- Maximum potential log grades: Various grade combinations, representing pulpwood through veneer quality, for the bottom three 8' bolts in each tree

#### Refined Dataset (data2.R)

Based on findings from the coarse analysis, a refined dataset was created with:

**Species** (7 total): Hard Maple (BM), Paper Birch (PB), Red Maple (RM), Red Oak (SO), White Oak (WO), White Pine (WP), Yellow Birch (YB)

**Sites** (4 total): granitictill, richnhw, mountaintill, clayplain

**Key refinements**:
- DBH: 10-28 inches in 1-inch increments (finer resolution)
- Only trees with one or more sawtimber- or veneer-grade bolts were included (pulpwood-, firewood-, and tie-log-only trees are never crop trees according to coarse analysis)

**Species excluded from final analysis based on coarse analysis**:
- Black Cherry (BC): Never a crop tree, under any circumstances.
- Balsam Fir (BF): Only ever a crop tree on the lowland spruce-fir site.
- Eastern Hemlock (EH): Only ever a crop tree on the lowland spruce-fir site.
- Red Spruce (RS): Only ever a crop tree on the lowland spruce-fir site.

**Site excluded**:
- `lowlandspfr`: Softwood-dominated site with very low LEV values (~$0.20/acre at 3%) and very low growth rates, making crop tree economics fundamentally different.

### Growth and Value Simulation (simulation.R)

Trees are grown forward in 5-year timesteps using the `ForestMaker` forest growth simulator (Foppert and Maker 2024), which uses task-specific XGBoost gradient boosting machines for:

1. **Diameter growth**: periodic diameter increment
2. **Height growth**: periodic height growth
3. **Survival**: Probabilistic tree survival
4. **Crown ratio change**: Periodic chanage derived from height growth and change in height to base of live crown models

GBMs and model parameters were tuned to US EPA Level II Ecoregion 5.3 (the Atlantic Highlands), which covers most of Vermont and New Hampshire, northwestern Maine, The Adirondacks and Catskills in New York, and parts of northern Pennsylvania.

`ForestMaker` is also used to track simulated individual (within-tree) log grades, volumes, mill price values, and stumpage values. A custom function is developed here that downgrades log grades based on trees' heights to the base of live crown, when maximum potential log grades for a particular bolt are not realized because of the growth or persistance of live branches.

Each tree is modeled as the only tree in an ~1/24 acre neighborhood, simulating a full release at the start of the simulation and subsequent growth in free-to-grow conditions (as occurs with a crop tree release treatment). 

**Mortality Assumption**:

'`ForestMaker` survival/mortality predictions do not account for individual tree health indicators, so mortality rates are reduced by a `mortality_factor` of 0.6 (60% of model-predicted mortality). This reflects the assumption that:

1. Foresters will only mark crop trees that appear healthy
2. Trees with visible crown dieback, disease symptoms, or defects will not be treated as crop trees.
3. This selective marking reduces realized mortality by approximately 40% (See separate lit review)

**Value calculation**:
- Log volumes are computed using the 1/4" International rule based on species-specific taper and bark thickness models built into `ForestMaker`, and log diameters inside bark at the small end.
- #2-grade sawtimber mill prices are based on January 2025 Log Street Journal reports, grouped by:
  - High-value hardwoods: $456/MBF
  - Medium-value hardwoods: $406/MBF
  - Pine/hemlock: $285/MBF
  - Softwoods: $370/MBF
  These species groupings encapsulate broad price trends that are likely to persist, rather than current prices that are subject to change relative to one another.
- Sawtimber mill prices for other log grades are modeled by `ForestMaker` based on #2 log mill prices and analysis of historic price patterns (see Foppert and Maker 2024).
- Roadside prices are mill prices minus $75/MBF trucking cost, based on consulting forestry records from Pekin Branch Forestry.  
- Stumpage rates = 0.6 × (roadside price - $60), based on experience of the authors.

**Key simulation outputs**:
- `value_nomort`: Tree value assuming survival
- `value`: Expected value accounting for cumulative survival probability
- Values are computed at each 5-year timestep from year 0 to year 250 or until DBH reaches 40"

### Land Expectation Value Calculation (lev.R)

LEV represents the present value of bare land assuming an infinite series of optimal rotations. It captures the opportunity cost of not harvesting the current tree and starting fresh.

**Calculation approach**:
1. Define a set of "replacement trees" for each site, representing trees that are likely to regenerate following harvest and to be retained through the susequent rotation. Define trees based on species, initial DBH, crown ratio, and grade.
2. Specify weights for each replacement tree within each site, correlating to the liklihoods of each being established and retained. Weights of all replacement trees for a site sum to 1.
3. Simulate each replacement tree's growth and development over the subsequent rotation and calculate NPV at each timestep, accross each of 5 different discount rates (2-6%).
4. Find the tree age at which NPV is maximized for each tree / discount rate combination (the expected rotation age) and calulate the expected LEV by entering the simulated stumpage value at the expected rotation age into Faustmann's formula.
5. Itteratively decrease the expected rotation length and recalculate the expected LEV, until the expected LEV stops rising and starts falling. The maximal expected LEV is the true LEV for a given replacement tree.
5. For each site / discount rate combination, calculate the final LEV as the average of the replacement trees' LEVs, weighted by their previously specified weights.

**LEV values at 3% discount rate** (representative):
- granitictill: $5.63/acre
- richnhw: $11.44/acre
- mountaintill: $7.93/acre
- lowlandspfr: $0.19/acre
- clayplain: $3.79/acre

### Crop Tree Classification (fct-croptrees.R)

A tree is classified as a crop tree if retaining it generates higher NPV than harvesting immediately.

**Decision criterion**:
```
crop = TRUE if:
  max(PV[t]) > PV[t=0] AND
  max(PV[t]) > PV[t=5] AND
  max(PV[t]) > PV[t=10]
```

Where: `PV[t] = (value[t] + LEV) / (1 + r)^t`

This requires that the optimal harvest time is at least 15 years in the future—a practical threshold ensuring the tree is worth growing for a meaningful period.

**Parameters**:
- `rate`: Discount rate (tested at 2%, 3%, 4%, 5%, 6%)
- `price_factor`: Multiplier on stumpage values for price sensitivity
- `mortality_factor`: Multiplier on mortality rates (base analysis uses 0.6, assuming foresters select healthy trees)
- `lev_factor`: Multiplier on LEV values for sensitivity analysis

### Coarse Analysis with Random Forest (coarse-analysis.R)

Before fitting interpretable decision trees, Random Forest models were used to:

1. **Identify important predictors** via variable importance (mean decrease in Gini impurity)
2. **Explore interactions** via partial dependence plots
3. **Guide decision tree structure** by understanding which variables matter most which set of decision trees will represent the decision space most efficiently

Log grade profiles were decomposed into different, easier-to-interpret variables, to determine which are most informative. These include: grade of best bolt, grade of worst bolt, grade of butt (bottomost) bolt, presence of veneer (boolean), presence of sawtimber (boolean), and quality score (in which bottom three bolts were scored based on their grades and then averaged, with bottom bolts weighted 2.5x more than top bolts and middle bolts weighted 1.5x more than top bolts; tie-quality bolts scored 1, sawtimber 2, veneer 3, and other grades 0).

**Key findings from variable importance**:

*Hardwoods*:
1. Species (most important)
2. DBH
3. Crown ratio
4. Site
5. Best log grade
6. Quality score

*Softwoods*:
1. Site (most important)
2. DBH
3. Crown ratio
4. Species
5. Quality score

**Partial dependence insights**:
- **DBH**: Probability of being a crop tree decreases sharply above ~14-16" for most species
- **Crown ratio**: Positive relationship—trees with higher CR are more likely to be crop trees, with threshold effects around 30-40%
- **DBH × CR interaction**: Strong, species-specific interaction—high CR trees can be crop trees at larger DBH
- **Grade effects**: Veneer-quality trees (grade 1) have substantially higher crop tree probability than sawtimber (grade 2), and crop tree probability drops precipitously when trees only have tie- or pulp-quality.

### Timber Quality Analysis (grades-analysis.R)

See separate report.

### Decision Tree Fitting (fct-decision-tree.R)

For each species/site/discount rate combination, a decision tree was fit using the `rpart` package:

**Model specification**:
- Predictors: DBH, crown ratio, best log grade
- Response: Crop tree (TRUE/FALSE)
- Complexity parameter tuned for interpretability (limited tree depth to 5, required at least 10 observations in each terminal node and at least 20 observations to consider splitting, pruned away splits that didn't improve fit by at least 1%)

**Output**:
- Decision tree objects (`.rds` files)
- Visual rule diagrams (`.png` files)
- Special handling for edge cases (all crop, no crop)

### Simplified Rule Generation (summarize-decision-trees.R)

Decision tree rules were extracted and summarized into field-usable prose:

**Process**:
1. Extract split rules from each decision tree
2. Identify patterns across sites and discount rates
3. Calculate base thresholds at 2% discount rate
4. Compute discount rate adjustments (DBH change per 1% increase)
5. Compute site adjustments (deviation from mean)
6. Generate prose summaries by species

**Rule structure** (common pattern):
```
Base rule (at 2% discount rate):
- If crown ratio >= X%: crop tree if DBH <= Y"
- If crown ratio < X%: crop tree if DBH <= Z"
- Veneer bonus: +N" for veneer-quality trees

Discount rate adjustment: -M" per 1% increase from 2%
Site adjustments: site1 +A", site2 -B", etc.
```

### Rule Accuracy Evaluation (evaluate-simplified-rules.R)

Simplified rules were evaluated against the original decision trees and training data:

**Metrics computed**:
- Accuracy: Overall classification agreement
- Precision: Proportion of predicted crop trees that are actual crop trees
- Recall: Proportion of actual crop trees correctly identified
- F1 Score: Harmonic mean of precision and recall
- False Positive Rate (FPR): Non-crop trees incorrectly classified as crop
- False Negative Rate (FNR): Crop trees incorrectly classified as non-crop

**Evaluation approach**:
- Rules are dynamically derived from simplified_rules.csv
- Applied to the full training dataset
- Compared to both original decision tree predictions and actual crop tree status

### Sensitivity Analysis (sensitivity-analysis.R)

The sensitivity of crop tree classifications to various assumptions was tested:

**Factors varied**:
- `mortality_factor`: 0.4, 0.6 (base), 0.8, 1.0
- `lev_factor`: 0.5, 0.75, 1.0 (base), 1.25, 1.5
- `price_factor`: 0.5, 0.75, 1.0 (base), 1.5, 2.0

**Metrics**:
- Proportion of trees classified as crop
- Classification agreement with base scenario
- Elasticity: % change in outcome / % change in factor

**Interpretation**:
- Higher `mortality_factor` → fewer crop trees (higher risk of loss)
- Higher `lev_factor` → fewer crop trees (higher opportunity cost)
- Higher `price_factor` → more crop trees (greater future value)

---

## Results

### Key Drivers of Crop Tree Classification

Based on the coarse analysis and decision tree patterns, the following factors drive crop tree decisions in order of importance:

1. **Species**: Different species have fundamentally different dynamics:
   - High-value and fast growing species (like yellow birch) can be crop trees at larger sizes
   - Lower-value and slower growing species have tighter DBH thresholds
   - White pine and other softwoods are never crop trees under the economic assumptions used, except on very poor sites like the lowland spruce-fir site.
   - Black cherries are never crop trees on any site, because of their high mortality risk.

2. **DBH (Diameter at Breast Height)**: Larger trees are less likely to be crop trees because:
   - They have less growth potential remaining
   - The capital tied up in them has higher opportunity cost
   - Mortality risk accumulates over the longer time needed to add meaningful value

3. **Crown Ratio**: Trees with larger crowns are more likely to be crop trees:
   - Higher CR indicates better vigor and growth potential
   - CR thresholds typically around 25-35%
   - CR interacts with DBH—high CR trees can be retained at larger sizes

4. **Site**: Site quality affects growth rates and thus crop tree thresholds:
   - Better sites (richnhw) generally allow slightly larger crop trees
   - Poorer sites (granitictill) have tighter thresholds
   - Different sites affect different species in different ways. For example, red oak grows best on well-drained soils and behaves similarly on all three till soils, but is less likely to be a crop tree on poorly-drained clayplain soils, while red maple thrives on finely-textured soils and is much more likely to be a crop tree on clayplain than on sandy granitic till.
   - Site effects are typically 1-2" adjustments

5. **Best Log Grade**: Timber quality affects crop tree potential:
   - Trees without sawtimber or veneer are generally not crop trees
   - Veneer-quality trees (grade 1) typically add 2-5" to the maximum crop tree DBH
   - The presence of veneer matters much more to some species (eg. paper birch) than others (eg. yellow birch)

6. **Discount Rate**: Higher discount rates lead to tighter crop tree criteria:
   - Each 1% increase in discount rate typically reduces max crop DBH by 0.8-2.3" depending on species
   - At 6% discount, few trees are worth retaining

### Common Interactions

**DBH × Crown Ratio**: The most important interaction. High crown ratio trees can be retained at substantially larger diameters. For example:
- Sugar maple at 2%: CR ≥ 30% allows DBH ≤ 17"; CR < 30% limits to DBH ≤ 15"
- Yellow birch shows even stronger CR effects

**DBH × Grade**: Veneer-quality trees have higher value ceilings, allowing retention at larger sizes, but the effects depend on species:
- Paper birch and red maple: Veneer bonus of 4" above sawlog thresholds
- Sugar maple and Oaks: 2-3" veneer bonus 
- Yellow birch: No veneer bonus

**Species × Site**: Some species show stronger site effects than others:
- Yellow birch: ±2.5" site adjustment, seems to be graded along nutrient availability, with highest crop tree rates on rich nhw and lowest on sandy till, irrespective of moisture regime
- Sugar maple: Site effects more modest (±0.8"), appears to be based on water availability (lower crop tree rate on xeric sandy till)
- Paper birch: moderate site effects (±1"), highest crop tree rates on moderately well-drained soils (mountaintill and richnhw), moderate on very well-drained soils (granitictill), and low on poorly-drained soils (clayplain).
- Red maple: moderate site effects (±1"), cleanly graded along soil drainage, with lower crop tree rates on more well drained soils
- Oaks: low site effects (±0.4"), crop tree rates slightly higher on clayplain than elsewhere

### Species-Specific Findings

#### Sugar Maple (BM)
- Moderate crop tree potential
- CR threshold around 30%
- Veneer bonus of ~2"
- Relatively insensitive to site
- Discount rate adjustment: ~1"/1%

#### Paper Birch (PB)
- Limited crop tree potential
- CR threshold around 35%
- Strong veneer bonus (+4")
- Strong site effects
- Discount rate adjustment: ~0.8"/1%
- Shorter-lived species increases mortality risk, particularly with smaller CR

#### Red Maple (RM)
- Moderate crop tree potential
- Veneer bonus of ~2"
- CR threshold around 30%
- Site-dependent
- Strong discount rate sensitivity (~1.3"/1%)

#### Red Oak (SO)
- Good crop tree potential 
- CR threshold around 30%
- Moderate veneer bonus (+3")
- Insensitive to site
- Strong discount rate sensitivity (~1.2"/1%)

#### White Oak (WO)
- Good crop tree potential 
- Large DBH thresholds (up to 21" at 2%)
- CR requirement of ≥ 25%
- Veneer bonus of ~2"
- Insensitive to site
- Strong discount rate sensitivity (~1.2"/1%)

#### Yellow Birch (YB)
- Best crop tree potential
- Largest DBH thresholds (up to 26")
- Very strong CR effects (CR ≥ 40% vs < 40%)
- Large site effects (±2.5")
- Strongest discount rate sensitivity (~2.3"/1%)

#### Black Cherry (BC)
- **Never a crop tree** under these economic assumptions
- Reflects high mortality rates

#### White Pine (WP)
- **Never a crop tree** under these economic assumptions, except on lowland spruce-fir sites
- smaller DBH WP w/ large CR and low mortality risk could be crop trees on other sites, at lower than expected LEV or if prices rise substantially, particularly on less productive sites like clayplain
- Reflects lower stumpage values and slower value appreciation relative to hardwoods

#### Eastern Hemlock (EH), Balsam Fir (BF), & Red Spruce (RS)
- **Never a crop tree** under these economic assumptions, except on lowland spruce-fir sites
- Reflects lower stumpage values and slower value appreciation relative to hardwoods


### Mortality Assumption

The final rules assume `mortality_factor = 0.6` (60% of model-predicted mortality). This reflects the assumption that:

1. Foresters will only mark crop trees that appear healthy
2. Trees with visible crown dieback, disease symptoms, or defects will not be treated as crop trees.
3. This selective marking reduces realized mortality by approximately 40% (See separate lit review)

Without this adjustment, rules would be substantially more conservative (smaller DBH thresholds).

---

## Final Simplified Rules

The following rules are for a **2% discount rate**. Adjust DBH thresholds downward by the indicated amount for each 1% increase in discount rate.

### Sugar Maple (BM)
**Base rule:**
Crop tree if sawtimber- or veneer-quality AND
- Crown ratio ≥ 30%: Crop tree if DBH ≤ 17"
- Crown ratio < 30%: Crop tree if DBH ≤ 15"
- Veneer-quality trees: +2" to threshold

**Adjustments:**
- Discount rate: -1" per 1% increase from 2%
- Site: granitictill -0.8"

### Paper Birch (PB)
**Base rule:**
Crop tree if sawtimber- or veneer-quality AND
- Crown ratio ≥ 35%: Crop tree if DBH ≤ 18"
- Crown ratio < 35%: Crop tree if DBH ≤ 16"
- Veneer bonus: +4" for veneer-quality

**Adjustments:**
- Discount rate: -0.8" per 1% increase from 2%
- Sites: mountaintill +0.8", richnhw +0.8", clayplain -1.2"

### Red Maple (RM)
**Base rule:**
Crop tree if sawtimber- or veneer-quality AND
- Crown ratio ≥ 30%: Crop tree if DBH ≤ 20"
- Crown ratio < 30%: Crop tree if DBH ≤ 17"
- Veneer bonus: +4" for veneer-quality

**Adjustments:**
- Discount rate: -1.3" per 1% increase from 2%
- Sites: clayplain +1", granitictill -1"

### Red Oak (SO)
**Base rule:**
Crop tree if sawtimber- or veneer-quality AND
- Crown ratio ≥ 30%: Crop tree if DBH ≤ 19"
- Crown ratio < 30%: Crop tree if DBH ≤ 16"
- Veneer bonus: +3" for veneer-quality

**Adjustments:**
- Discount rate: -1.2" per 1% increase from 2%
- Site: clayplain +0.8"

### White Oak (WO)
**Base rule:**
- Crop tree if DBH ≤ 19" (sawtimber) or ≤ 21" (veneer)
- Requires crown ratio ≥ 25%

**Adjustments:**
- Discount rate: -1.2" per 1% increase from 2%
- Site: clayplain +0.8"

### Yellow Birch (YB)
**Base rule:**
Crop tree if sawtimber- or veneer-quality AND
- Crown ratio ≥ 40%: Crop tree if DBH ≤ 26"
- Crown ratio < 40%: Crop tree if DBH ≤ 17"

**Adjustments:**
- Discount rate: -2.3" per 1% increase from 2%
- Sites: richnhw +2.5", clayplain +1.5", mountaintill -1.5", granitictill -2.5"

### Black Cherry (BC)
**Never a crop tree** under these economic assumptions.

### White Pine (WP)
**Never a crop tree** under these economic assumptions. 

---

## Rule Accuracy

Performance of simplified rules versus original decision trees (all discount rates combined):

| Species | Simplified Accuracy | Tree Accuracy | Simplified F1 | Tree F1 |
|---------|--------------------:|-------------:|-------------:|--------:|
| BM | 92.6% | 96.3% | 83.9% | 90.9% |
| PB | 88.1% | 94.7% | 68.2% | 80.9% |
| RM | 88.5% | 93.5% | 80.9% | 88.1% |
| SO | 91.1% | 93.8% | 85.2% | 89.4% |
| WO | 85.1% | 93.6% | 81.6% | 91.6% |
| YB | 81.1% | 96.3% | 79.7% | 96.4% |

**Interpretation:**
- Original decision trees prioritize **precision** (rarely marking non-crop trees as crop)
- Simplified rules trade some precision for higher **recall** (catching more actual crop trees)
- Overall, simplified rules achieve 88% accuracy with 80% F1 score

---

## Sensitivity Analysis Findings

Sensitivity analysis was conducted at a fixed 3% discount rate, varying mortality, LEV, and price factors independently from their base values. The base scenario uses `mortality_factor = 0.6`, `lev_factor = 1.0`, and `price_factor = 1.0`, which classifies 30.8% of trees as crop trees.

### Overall Effects on Crop Tree Proportion

| Factor | Value | % Crop Trees | Change from Base |
|--------|------:|-------------:|-----------------:|
| **Base** | — | 30.8% | — |
| Mortality | 0.4 | 34.7% | +3.9 pp |
| Mortality | 0.8 | 26.7% | -4.1 pp |
| Mortality | 1.0 | 22.9% | -7.9 pp |
| LEV | 0.5 | 34.0% | +3.2 pp |
| LEV | 0.75 | 32.4% | +1.6 pp |
| LEV | 1.25 | 29.2% | -1.6 pp |
| LEV | 1.5 | 27.7% | -3.1 pp |
| Price | 0.5 | 24.7% | -6.1 pp |
| Price | 0.75 | 28.7% | -2.1 pp |
| Price | 1.5 | 32.9% | +2.1 pp |
| Price | 2.0 | 34.0% | +3.2 pp |

### Classification Stability

Agreement rates measure what proportion of trees receive the same crop/non-crop classification under alternative assumptions as under base assumptions:

| Factor | Value | Agreement | Switch to Crop | Switch to Non-Crop |
|--------|------:|----------:|---------------:|-------------------:|
| Mortality | 0.4 | 93.1% | 5.5% | 1.4% |
| Mortality | 0.8 | 92.9% | 1.5% | 5.6% |
| Mortality | 1.0 | 90.3% | 0.8% | 9.0% |
| LEV | 0.5 | 93.6% | 4.7% | 1.7% |
| LEV | 0.75 | 94.2% | 3.7% | 2.1% |
| LEV | 1.25 | 93.9% | 2.3% | 3.8% |
| LEV | 1.5 | 93.1% | 1.9% | 4.9% |
| Price | 0.5 | 91.3% | 1.3% | 7.4% |
| Price | 0.75 | 93.7% | 2.1% | 4.2% |
| Price | 1.5 | 94.1% | 4.0% | 1.9% |
| Price | 2.0 | 93.6% | 4.7% | 1.7% |

### Interpretation

**Mortality factor** has the strongest effect on crop tree classification:
- Reducing mortality by an additional 20% (from 0.6 to 0.4) increases crop trees by 3.9 percentage points
- Using full predicted mortality (factor = 1.0, equivalent to a forester being blind to tree health indicators) reduces crop trees by 7.9 percentage points and causes the largest classification instability (90.3% agreement)
- This underscores the importance of selecting visibly healthy trees for crop tree treatment

**Price factor** has moderate effects:
- Halving prices (-50%) reduces crop trees by 6.1 percentage points
- Doubling prices (+100%) increases crop trees by 3.2 percentage points
- The asymmetric response (larger effect for price decreases) suggests the base price assumptions may be near an inflection point for some tree/site combinations

**LEV factor** has the smallest effect:
- This is expected given that LEV values are small ($4-11 at 3%) relative to individual tree values ($0-680)
- Halving LEV increases crop trees by only 3.2 percentage points

### Species-Specific Elasticities

Elasticity measures the percentage change in crop tree proportion per percentage change in the factor. Species respond differently to economic assumptions:

| Species | Mortality Elasticity | LEV Elasticity | Price Elasticity |
|---------|---------------------:|---------------:|-----------------:|
| BM | -0.14 | -0.07 | +0.08 |
| PB | -0.15 | -0.12 | +0.12 |
| RM | -0.16 | -0.11 | +0.12 |
| SO | -0.14 | -0.07 | +0.09 |
| WO | -0.11 | -0.04 | +0.05 |
| WP | -0.00 | -0.02 | +0.01 |
| YB | -0.14 | -0.01 | +0.02 |

**Key patterns:**
- All species show negative mortality elasticity (higher mortality → fewer crop trees)
- Paper birch (PB) and red maple (RM) are most sensitive to price and LEV changes
- White oak (WO) and yellow birch (YB) are relatively insensitive to price and LEV
- White pine (WP) shows minimal sensitivity because it's rarely a crop tree under any scenario

---

## Limitations and Caveats

1. **Appropriate use**: Crop tree rules do not account for ongoing competition between residual trees, site-specific regeneration dynamics, the presence of interfering vegetation, or non-timber objectives, and should not be used as a simplistic guide for which trees to cut and which to keep. Potential crop trees may be removed before maturity to release other crop trees, take advantage of favorable regeneration conditions, or achieve other goals, and non-crop trees may be retained to control interfering vegetation or limit epicormic sprouting of neighboring crop trees, for example. 

2. **Free-to-grow assumption**: Analysis assumes crop trees remain free-to-grow from release onward. This requires residual overstory stocking below c-line or ongoing tending.

3. **Individual tree health**: Rules do not directly account for individual tree health indicators (crown dieback, disease, damage). The 60% mortality factor partially addresses this by assuming foresters select healthy trees.

4. **Price stability**: Rules assume relative price relationships between species and grades remain stable over time, even though they are likely to change. Using species groupings (high value hardwoods, medium value hardwoods, etc.) partly addresses this by capturing long-term observed price trends.

5. **Regional applicability**: Rules are calibrated for US EPA Level II Ecoregion 5.3 and may not apply elsewhere.

6. **Discount rate selection**: Appropriate discount rate depends on landowner objectives and alternatives, although when there are few barriers to land transfer, land will end up in the hands of owners whose discount rate preferences match forests' inherent risk-adjusted rates of return. The 2-6% range covers most private forest management scenarios.

7. **Rule simplification**: Simplified prose rules sacrifice some accuracy for field usability. For high-stakes decisions, original decision trees are more reliable.

---

## File Structure

```
croptrees/
├── R/
│   ├── data.R                    # Initial broad dataset
│   ├── data2.R                   # Refined focused dataset
│   ├── simulation.R              # Growth and value simulation
│   ├── lev.R                     # Land Expectation Value calculation
│   ├── fct-croptrees.R           # Crop tree classification function
│   ├── coarse-analysis.R         # Random Forest exploration
│   ├── grades-analysis.R         # Timber quality analysis
│   ├── fct-decision-tree.R       # Decision tree fitting
│   ├── summarize-decision-trees.R # Rule extraction and simplification
│   ├── evaluate-simplified-rules.R # Accuracy evaluation
│   ├── sensitivity-analysis.R    # Sensitivity testing
│   └── run.R                     # Master script
├── output/
│   ├── decision-trees/
│   │   ├── dataset2/             # Initial decision trees from broad dataset  
│   │   └── dataset2/             # Final decision trees and simplified rules, and accuracy
│   ├── base-rate-rf-analysis/    # Coarse analysis outputs
│   ├── replacement-tree-lev/     # LEV calculation details
│   └── sensitivity-analysis/     # Sensitivity results
├── mortality-adjustment.odt      # Describes mortality adjustment reasoning
└── PROJECT_DOCUMENTATION.md      # This document
```

---

## Citation

[To be added when published]

## Contact

Neal Maker
neal@forestbiometrics.org
