# sensitivity-analysis.R
#
# Sensitivity analysis for crop tree rules across mortality_factor, lev_factor,
# and price_factor. Compares results to base rules from dataset2.
#
# This script should be run AFTER the base analysis (run.R) has completed and
# the dataset2 results exist.
#
# Usage:
#   source("R/sensitivity-analysis.R")

library(dplyr)
library(tidyr)
library(rpart)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Reference discount rate for sensitivity analysis
REFERENCE_DRATE <- 0.03

# Base factor values (what the base rules use)
BASE_MORTALITY_FACTOR <- 0.6
BASE_LEV_FACTOR <- 1.0
BASE_PRICE_FACTOR <- 1.0

# Sensitivity levels to test
MORTALITY_FACTORS <- c(0.4, 0.6, 0.8, 1.0)
LEV_FACTORS <- c(0.5, 0.75, 1.0, 1.25, 1.5)
PRICE_FACTORS <- c(0.5, 0.75, 1.0, 1.5, 2.0)

# Output directory
OUTPUT_DIR <- "output/sensitivity-analysis"

# =============================================================================
# SETUP
# =============================================================================

message("=== Crop Tree Sensitivity Analysis ===\n")

# Create output directory
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# Check that required objects exist or load them
if (!exists("sim") || !exists("lev_lookup")) {
  message("Loading simulation data and LEV lookup...")

  # Need to source the data preparation scripts
  if (!exists("stupper")) stupper <- TRUE
  if (!exists("pulppositive")) pulppositive <- FALSE
  if (!exists("drates")) drates <- seq(0.02, 0.06, by = 0.01)

  source("R/data2.R")
  source("R/simulation.R")
  source("R/lev.R")
}

source("R/fct-croptrees.R")

message("Simulation data: ", nrow(sim$trees), " tree-year observations")
message("Reference discount rate: ", REFERENCE_DRATE * 100, "%")

# Load base simplified rules for comparison
base_rules_file <- "output/decision-trees/dataset2/simplified_rules.csv"
if (!file.exists(base_rules_file)) {
  stop("Base rules not found. Run the base analysis first (run.R)")
}
base_rules <- read.csv(base_rules_file)

# Filter base rules to reference discount rate
base_rules_ref <- base_rules %>%
  filter(abs(drate - REFERENCE_DRATE) < 0.005)

message("Base rules loaded: ", nrow(base_rules_ref), " species/site combinations at ",
        REFERENCE_DRATE * 100, "% discount rate\n")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Run crop_trees and extract DBH thresholds
#'
#' Generates crop tree classifications and extracts effective DBH thresholds
#' for each species/site combination using multiple methods.
#'
#' @param sim_data Simulation tree data
#' @param drate Discount rate
#' @param price_factor Price multiplier
#' @param mortality_factor Mortality multiplier
#' @param lev_factor LEV multiplier
#' @param levs LEV lookup table
#' @return Data frame with DBH thresholds per species/site
extract_thresholds <- function(sim_data, drate, price_factor, mortality_factor,
                                lev_factor, levs) {

  # Generate crop tree classifications
  crops <- crop_trees(sim_data, drate, price_factor, levs,
                      mortality_factor = mortality_factor,
                      lev_factor = lev_factor)

  # For each species/site, calculate multiple threshold measures
  thresholds <- crops %>%
    group_by(spp, site) %>%
    summarise(
      n_trees = n(),
      n_crop = sum(crop),
      prop_crop = mean(crop),
      # Max DBH among crop trees
      max_crop_dbh = ifelse(sum(crop) > 0, max(dbh[crop]), NA_real_),
      # Weighted mean DBH of crop trees (center of mass)
      mean_crop_dbh = ifelse(sum(crop) > 0, mean(dbh[crop]), NA_real_),
      # DBH at which crop probability crosses 50% (interpolated)
      # Calculate prop_crop at each DBH level and find crossover
      dbh_50pct = {
        if (sum(crop) == 0 || sum(crop) == n()) {
          NA_real_
        } else {
          # Calculate cumulative proportion crop from high DBH to low
          dbh_levels <- sort(unique(dbh), decreasing = TRUE)
          cum_crop <- sapply(dbh_levels, function(d) mean(crop[dbh <= d]))
          # Find where it crosses 0.5
          idx <- which(cum_crop >= 0.5)
          if (length(idx) == 0) {
            max(dbh)
          } else {
            dbh_levels[max(idx)]
          }
        }
      },
      .groups = "drop"
    )

  # Use dbh_50pct as the primary threshold measure, fall back to max_crop_dbh
  thresholds <- thresholds %>%
    mutate(
      effective_dbh_threshold = coalesce(dbh_50pct, max_crop_dbh, 0)
    )

  thresholds
}

#' Calculate classification agreement between two scenarios
#'
#' @param crops_base Crop tree data from base scenario
#' @param crops_test Crop tree data from test scenario
#' @return Data frame with agreement metrics
calculate_agreement <- function(crops_base, crops_test) {

  # Merge by tree identifier
  merged <- crops_base %>%
    select(spp, site, dbh, cr, best_log, crop_base = crop) %>%
    inner_join(
      crops_test %>% select(spp, site, dbh, cr, best_log, crop_test = crop),
      by = c("spp", "site", "dbh", "cr", "best_log")
    )

  # Calculate agreement metrics
  agreement <- merged %>%
    group_by(spp, site) %>%
    summarise(
      n = n(),
      agree_crop = sum(crop_base & crop_test),
      agree_noncrop = sum(!crop_base & !crop_test),
      switch_to_crop = sum(!crop_base & crop_test),
      switch_to_noncrop = sum(crop_base & !crop_test),
      agreement_rate = (agree_crop + agree_noncrop) / n,
      .groups = "drop"
    )

  agreement
}

# =============================================================================
# RUN SENSITIVITY SCENARIOS
# =============================================================================

message("=== Running Sensitivity Scenarios ===\n")

# Store all results
all_thresholds <- list()
all_crops <- list()

# Generate base case (at reference discount rate with base factors)
message("Running base case...")
crops_base <- crop_trees(sim$trees, REFERENCE_DRATE, BASE_PRICE_FACTOR, lev_lookup,
                         mortality_factor = BASE_MORTALITY_FACTOR,
                         lev_factor = BASE_LEV_FACTOR)
thresholds_base <- extract_thresholds(sim$trees, REFERENCE_DRATE, BASE_PRICE_FACTOR,
                                       BASE_MORTALITY_FACTOR, BASE_LEV_FACTOR, lev_lookup)
thresholds_base$scenario <- "base"
thresholds_base$factor_type <- "base"
thresholds_base$factor_value <- 1.0
all_thresholds[["base"]] <- thresholds_base
all_crops[["base"]] <- crops_base

# --- Mortality Factor Sensitivity ---
message("\nMortality factor sensitivity...")
for (mf in MORTALITY_FACTORS) {
  if (mf == BASE_MORTALITY_FACTOR) next  # Skip base case

  scenario_name <- paste0("mortality_", mf)
  message("  mortality_factor = ", mf)

  crops <- crop_trees(sim$trees, REFERENCE_DRATE, BASE_PRICE_FACTOR, lev_lookup,
                      mortality_factor = mf, lev_factor = BASE_LEV_FACTOR)
  thresholds <- extract_thresholds(sim$trees, REFERENCE_DRATE, BASE_PRICE_FACTOR,
                                    mf, BASE_LEV_FACTOR, lev_lookup)
  thresholds$scenario <- scenario_name
  thresholds$factor_type <- "mortality"
  thresholds$factor_value <- mf

  all_thresholds[[scenario_name]] <- thresholds
  all_crops[[scenario_name]] <- crops
}

# --- LEV Factor Sensitivity ---
message("\nLEV factor sensitivity...")
for (lf in LEV_FACTORS) {
  if (lf == BASE_LEV_FACTOR) next  # Skip base case

  scenario_name <- paste0("lev_", lf)
  message("  lev_factor = ", lf)

  crops <- crop_trees(sim$trees, REFERENCE_DRATE, BASE_PRICE_FACTOR, lev_lookup,
                      mortality_factor = BASE_MORTALITY_FACTOR, lev_factor = lf)
  thresholds <- extract_thresholds(sim$trees, REFERENCE_DRATE, BASE_PRICE_FACTOR,
                                    BASE_MORTALITY_FACTOR, lf, lev_lookup)
  thresholds$scenario <- scenario_name
  thresholds$factor_type <- "lev"
  thresholds$factor_value <- lf

  all_thresholds[[scenario_name]] <- thresholds
  all_crops[[scenario_name]] <- crops
}

# --- Price Factor Sensitivity ---
message("\nPrice factor sensitivity...")
for (pf in PRICE_FACTORS) {
  if (pf == BASE_PRICE_FACTOR) next  # Skip base case

  scenario_name <- paste0("price_", pf)
  message("  price_factor = ", pf)

  crops <- crop_trees(sim$trees, REFERENCE_DRATE, pf, lev_lookup,
                      mortality_factor = BASE_MORTALITY_FACTOR, lev_factor = BASE_LEV_FACTOR)
  thresholds <- extract_thresholds(sim$trees, REFERENCE_DRATE, pf,
                                    BASE_MORTALITY_FACTOR, BASE_LEV_FACTOR, lev_lookup)
  thresholds$scenario <- scenario_name
  thresholds$factor_type <- "price"
  thresholds$factor_value <- pf

  all_thresholds[[scenario_name]] <- thresholds
  all_crops[[scenario_name]] <- crops
}

# =============================================================================
# COMBINE AND ANALYZE RESULTS
# =============================================================================

message("\n=== Analyzing Results ===\n")

# Combine all threshold data
thresholds_df <- bind_rows(all_thresholds)

# Calculate shifts relative to base
base_thresholds <- thresholds_df %>%
  filter(scenario == "base") %>%
  select(spp, site,
         base_dbh = effective_dbh_threshold,
         base_prop_crop = prop_crop,
         base_mean_crop_dbh = mean_crop_dbh)

threshold_shifts <- thresholds_df %>%
  filter(scenario != "base") %>%
  left_join(base_thresholds, by = c("spp", "site")) %>%
  mutate(
    dbh_shift = effective_dbh_threshold - base_dbh,
    dbh_pct_change = ifelse(base_dbh > 0,
                            (effective_dbh_threshold - base_dbh) / base_dbh * 100,
                            NA_real_),
    prop_crop_change = prop_crop - base_prop_crop,
    mean_crop_dbh_shift = mean_crop_dbh - base_mean_crop_dbh
  )

# =============================================================================
# CALCULATE ELASTICITIES
# =============================================================================

message("Calculating elasticities...")

# Elasticity = (% change in outcome) / (% change in factor)
# We calculate two versions:
# 1. Based on mean DBH of crop trees (more interpretable)
# 2. Based on proportion of trees classified as crop

calc_elasticity <- function(df, factor_type_filter, base_factor_value) {
  df %>%
    filter(factor_type == factor_type_filter) %>%
    mutate(
      factor_pct_change = (factor_value - base_factor_value) / base_factor_value * 100,
      # DBH-based elasticity
      dbh_elasticity = ifelse(
        factor_pct_change != 0 & !is.na(dbh_pct_change),
        dbh_pct_change / factor_pct_change,
        NA_real_
      ),
      # Prop-crop based elasticity (change in prop per % change in factor)
      prop_elasticity = ifelse(
        factor_pct_change != 0,
        (prop_crop_change * 100) / factor_pct_change,  # Convert to percentage points
        NA_real_
      )
    ) %>%
    group_by(spp) %>%
    summarise(
      factor_type = first(factor_type),
      mean_dbh_elasticity = mean(dbh_elasticity, na.rm = TRUE),
      mean_prop_elasticity = mean(prop_elasticity, na.rm = TRUE),
      mean_prop_crop_change = mean(prop_crop_change, na.rm = TRUE),
      mean_dbh_shift = mean(mean_crop_dbh_shift, na.rm = TRUE),
      .groups = "drop"
    )
}

elasticity_mortality <- calc_elasticity(threshold_shifts, "mortality", BASE_MORTALITY_FACTOR)
elasticity_lev <- calc_elasticity(threshold_shifts, "lev", BASE_LEV_FACTOR)
elasticity_price <- calc_elasticity(threshold_shifts, "price", BASE_PRICE_FACTOR)

elasticities <- bind_rows(elasticity_mortality, elasticity_lev, elasticity_price)

# =============================================================================
# CALCULATE CLASSIFICATION AGREEMENT
# =============================================================================

message("Calculating classification agreement...")

agreement_results <- list()

for (scenario_name in names(all_crops)) {
  if (scenario_name == "base") next

  agreement <- calculate_agreement(all_crops[["base"]], all_crops[[scenario_name]])
  agreement$scenario <- scenario_name

  # Extract factor info
  parts <- strsplit(scenario_name, "_")[[1]]
  agreement$factor_type <- parts[1]
  agreement$factor_value <- as.numeric(parts[2])

  agreement_results[[scenario_name]] <- agreement
}

agreement_df <- bind_rows(agreement_results)

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

message("Generating summary statistics...\n")

# Summary by factor type - focus on prop_crop as primary metric
summary_by_factor <- threshold_shifts %>%
  group_by(factor_type, factor_value) %>%
  summarise(
    n_spp_site = n(),
    # Proportion crop changes (most reliable metric)
    mean_prop_crop = mean(prop_crop, na.rm = TRUE),
    mean_prop_crop_change = mean(prop_crop_change, na.rm = TRUE),
    # DBH threshold changes (may be noisy)
    mean_dbh_shift = mean(dbh_shift, na.rm = TRUE),
    mean_mean_crop_dbh = mean(mean_crop_dbh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(factor_type, factor_value)

# Add base case to summary for reference
base_summary <- thresholds_df %>%
  filter(scenario == "base") %>%
  summarise(
    factor_type = "base",
    factor_value = 1.0,
    n_spp_site = n(),
    mean_prop_crop = mean(prop_crop, na.rm = TRUE),
    mean_prop_crop_change = 0,
    mean_dbh_shift = 0,
    mean_mean_crop_dbh = mean(mean_crop_dbh, na.rm = TRUE)
  )

summary_by_factor <- bind_rows(base_summary, summary_by_factor) %>%
  arrange(factor_type, factor_value)

# Summary by species - use prop_crop change as primary metric
summary_by_species <- threshold_shifts %>%
  group_by(spp, factor_type) %>%
  summarise(
    mean_prop_crop_change = mean(prop_crop_change, na.rm = TRUE),
    mean_dbh_shift = mean(dbh_shift, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = factor_type,
    values_from = c(mean_prop_crop_change, mean_dbh_shift),
    names_glue = "{factor_type}_{.value}"
  )

# Classification agreement summary
agreement_summary <- agreement_df %>%
  group_by(factor_type, factor_value) %>%
  summarise(
    mean_agreement = mean(agreement_rate, na.rm = TRUE),
    mean_switch_to_crop = mean(switch_to_crop / n, na.rm = TRUE),
    mean_switch_to_noncrop = mean(switch_to_noncrop / n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(factor_type, factor_value)

# =============================================================================
# PRINT RESULTS
# =============================================================================

message(paste(rep("=", 70), collapse = ""))
message("SENSITIVITY ANALYSIS RESULTS")
message(paste(rep("=", 70), collapse = ""))

cat("\n--- Summary by Factor Level ---\n")
cat("(mean_prop_crop = fraction of trees classified as crop)\n")
cat("(mean_prop_crop_change = change from base scenario)\n\n")
print(summary_by_factor, n = 50)

cat("\n--- Sensitivity by Species ---\n")
cat("(positive = more trees become crop, negative = fewer)\n\n")
print(summary_by_species, n = 20)

cat("\n--- Classification Agreement with Base Rules ---\n\n")
print(agreement_summary, n = 50)

cat("\n--- Elasticities (% change in mean crop DBH / % change in factor) ---\n\n")
print(elasticities %>% arrange(factor_type, spp), n = 30)

# =============================================================================
# INTERPRETATION GUIDANCE
# =============================================================================

cat("\n")
message(paste(rep("=", 70), collapse = ""))
message("INTERPRETATION")
message(paste(rep("=", 70), collapse = ""))

cat("\n")
cat("Factor interpretation:\n")
cat("  - mortality_factor: Lower values = healthier trees selected, more permissive rules\n")
cat("  - lev_factor: Higher values = more valuable replacement stand, tighter rules\n")
cat("  - price_factor: Higher values = higher future stumpage, more permissive rules\n")
cat("\n")
cat("Elasticity interpretation:\n")
cat("  - Positive elasticity: DBH threshold increases as factor increases\n")
cat("  - Negative elasticity: DBH threshold decreases as factor increases\n")
cat("  - |elasticity| > 1: DBH is highly sensitive to this factor\n")
cat("  - |elasticity| < 1: DBH is relatively insensitive to this factor\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

message("\n=== Saving Results ===\n")

# Save threshold data
write.csv(thresholds_df, file.path(OUTPUT_DIR, "sensitivity_thresholds.csv"),
          row.names = FALSE)
message("Saved: ", file.path(OUTPUT_DIR, "sensitivity_thresholds.csv"))

# Save threshold shifts
write.csv(threshold_shifts, file.path(OUTPUT_DIR, "sensitivity_shifts.csv"),
          row.names = FALSE)
message("Saved: ", file.path(OUTPUT_DIR, "sensitivity_shifts.csv"))

# Save elasticities
write.csv(elasticities, file.path(OUTPUT_DIR, "sensitivity_elasticities.csv"),
          row.names = FALSE)
message("Saved: ", file.path(OUTPUT_DIR, "sensitivity_elasticities.csv"))

# Save classification agreement
write.csv(agreement_df, file.path(OUTPUT_DIR, "sensitivity_agreement.csv"),
          row.names = FALSE)
message("Saved: ", file.path(OUTPUT_DIR, "sensitivity_agreement.csv"))

# Save summary tables
write.csv(summary_by_factor, file.path(OUTPUT_DIR, "summary_by_factor.csv"),
          row.names = FALSE)
write.csv(summary_by_species, file.path(OUTPUT_DIR, "summary_by_species.csv"),
          row.names = FALSE)
write.csv(agreement_summary, file.path(OUTPUT_DIR, "summary_agreement.csv"),
          row.names = FALSE)
message("Saved summary tables")

# Save as RDS for further analysis
saveRDS(list(
  thresholds = thresholds_df,
  shifts = threshold_shifts,
  elasticities = elasticities,
  agreement = agreement_df,
  summary_by_factor = summary_by_factor,
  summary_by_species = summary_by_species,
  agreement_summary = agreement_summary,
  config = list(
    reference_drate = REFERENCE_DRATE,
    base_mortality = BASE_MORTALITY_FACTOR,
    base_lev = BASE_LEV_FACTOR,
    base_price = BASE_PRICE_FACTOR,
    mortality_factors = MORTALITY_FACTORS,
    lev_factors = LEV_FACTORS,
    price_factors = PRICE_FACTORS
  )
), file.path(OUTPUT_DIR, "sensitivity_results.rds"))
message("Saved: ", file.path(OUTPUT_DIR, "sensitivity_results.rds"))

message("\nSensitivity analysis complete!")
