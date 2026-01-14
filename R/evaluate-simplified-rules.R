# evaluate-simplified-rules.R
#
# This script evaluates the accuracy of simplified prose rules against
# the original training data. It dynamically derives rules from the
# simplified_rules.csv file rather than hardcoding them.
#
# Usage:
#   source("R/evaluate-simplified-rules.R")
#   # Results will print to console and save to output directory

library(dplyr)
library(rpart)

# =============================================================================
# STEP 1: Load data
# =============================================================================

message("=== Loading training data ===\n")

# Get output directory from environment or use default
if (!exists("outdir")) outdir <- "dataset2"
tree_dir <- paste0("output/decision-trees/", outdir)

crops_file <- file.path(tree_dir, "crops_all.rds")
rules_file <- file.path(tree_dir, "simplified_rules.csv")

if (!file.exists(crops_file)) {
  stop("Training data not found at: ", crops_file)
}
if (!file.exists(rules_file)) {
  stop("Simplified rules not found at: ", rules_file)
}

crops_all <- readRDS(crops_file)
simplified_rules <- read.csv(rules_file)

message("Training data: ", nrow(crops_all), " observations")
message("Species: ", paste(unique(crops_all$spp), collapse = ", "))
message("Sites: ", paste(unique(crops_all$site), collapse = ", "))
message("Discount rates: ", paste(unique(crops_all$drate), collapse = ", "))

# =============================================================================
# STEP 2: Derive rule parameters from simplified_rules.csv
# =============================================================================

message("\n=== Deriving rule parameters ===\n")

# Reference discount rate (base rate for rules)
ref_drate <- 0.02

# For each species, calculate:
# 1. Base thresholds at reference rate (averaged across sites)
# 2. Discount rate adjustment (slope)
# 3. Site adjustments (deviation from mean at reference rate)

derive_rule_parameters <- function(rules_df, reference_rate = 0.02) {

  species_list <- unique(rules_df$spp)
  params <- list()

  for (sp in species_list) {
    sp_rules <- rules_df %>% filter(spp == sp)

    # 1. Get reference rate data
    ref_data <- sp_rules %>% filter(abs(drate - reference_rate) < 0.005)
    if (nrow(ref_data) == 0) {
      # Use closest available rate
      closest <- sp_rules$drate[which.min(abs(sp_rules$drate - reference_rate))]
      ref_data <- sp_rules %>% filter(abs(drate - closest) < 0.005)
    }

    # 2. Calculate base thresholds (mean across sites at reference rate)
    base_dbh_high_cr <- mean(ref_data$high_cr_dbh_max, na.rm = TRUE)
    base_dbh_low_cr <- mean(ref_data$low_cr_dbh_max, na.rm = TRUE)
    base_dbh_overall <- mean(ref_data$dbh_max_overall, na.rm = TRUE)
    base_cr_min <- mean(ref_data$cr_min_overall, na.rm = TRUE)

    # Use overall if specific CR thresholds not available
    if (is.na(base_dbh_high_cr) || !is.finite(base_dbh_high_cr)) {
      base_dbh_high_cr <- base_dbh_overall
    }
    if (is.na(base_dbh_low_cr) || !is.finite(base_dbh_low_cr)) {
      base_dbh_low_cr <- base_dbh_high_cr  # No CR effect
    }
    if (is.na(base_cr_min) || !is.finite(base_cr_min)) {
      base_cr_min <- 0  # No minimum CR
    }

    # 3. Check if grade matters
    grade_matters <- any(ref_data$grade_matters, na.rm = TRUE)
    veneer_dbh_max <- mean(ref_data$veneer_dbh_max, na.rm = TRUE)
    sawlog_dbh_max <- mean(ref_data$sawlog_dbh_max, na.rm = TRUE)

    # Calculate veneer bonus
    veneer_bonus <- 0
    if (grade_matters && !is.na(veneer_dbh_max) && !is.na(sawlog_dbh_max)) {
      veneer_bonus <- veneer_dbh_max - sawlog_dbh_max
    } else if (grade_matters && !is.na(veneer_dbh_max) && !is.na(base_dbh_high_cr)) {
      veneer_bonus <- veneer_dbh_max - base_dbh_high_cr
    }
    if (is.na(veneer_bonus) || !is.finite(veneer_bonus)) veneer_bonus <- 0

    # 4. Calculate discount rate slope (DBH change per unit drate)
    # Fit linear model across all sites
    drate_model <- lm(dbh_max_overall ~ drate, data = sp_rules)
    drate_slope <- coef(drate_model)[2]  # Change in DBH per unit change in drate
    if (is.na(drate_slope)) drate_slope <- 0

    # 5. Calculate site adjustments (deviation from mean at reference rate)
    site_adjustments <- ref_data %>%
      mutate(
        site_mean = mean(dbh_max_overall, na.rm = TRUE),
        adjustment = dbh_max_overall - site_mean
      ) %>%
      select(site, adjustment) %>%
      distinct()

    # Store parameters
    params[[sp]] <- list(
      base_dbh_high_cr = base_dbh_high_cr,
      base_dbh_low_cr = base_dbh_low_cr,
      cr_threshold = 30,  # Typical threshold from rules
      base_cr_min = base_cr_min,
      grade_matters = grade_matters,
      veneer_bonus = max(0, veneer_bonus),
      drate_slope = drate_slope,
      reference_rate = reference_rate,
      site_adjustments = setNames(site_adjustments$adjustment, site_adjustments$site)
    )
  }

  params
}

rule_params <- derive_rule_parameters(simplified_rules, ref_drate)

# Print derived parameters
message("Derived rule parameters:\n")
for (sp in names(rule_params)) {
  p <- rule_params[[sp]]
  cat(sprintf("%s: base DBH=%.1f\" (high CR) / %.1f\" (low CR), CR min=%.0f%%, ",
              sp, p$base_dbh_high_cr, p$base_dbh_low_cr, p$base_cr_min))
  cat(sprintf("veneer bonus=%.1f\", drate slope=%.1f\"/1%%\n",
              p$veneer_bonus, p$drate_slope * 100))
}

# =============================================================================
# STEP 3: Apply derived rules to predict crop trees
# =============================================================================

message("\n=== Applying simplified rules ===\n")

# Generic rule application function
apply_simplified_rule <- function(dbh, cr, best_log, site, drate, params) {
  # Get base DBH threshold based on CR
  # Use typical CR threshold of 30% (could be made species-specific)
  cr_thresh <- 30

  base_dbh <- ifelse(cr >= cr_thresh, params$base_dbh_high_cr, params$base_dbh_low_cr)

  # Apply discount rate adjustment
  drate_adj <- params$drate_slope * (drate - params$reference_rate)

  # Apply site adjustment
  site_adj <- ifelse(site %in% names(params$site_adjustments),
                     params$site_adjustments[site], 0)

  # Apply veneer bonus
  grade_bonus <- ifelse(params$grade_matters & best_log == 1, params$veneer_bonus, 0)

  # Calculate final max DBH
  max_dbh <- base_dbh + drate_adj + site_adj + grade_bonus

  # Apply CR minimum if specified
  cr_ok <- cr >= params$base_cr_min

  # Predict crop tree
  (dbh <= max_dbh) & cr_ok
}

# Apply rules to each observation
crops_all$simplified_pred <- NA

for (sp in names(rule_params)) {
  idx <- crops_all$spp == sp
  if (sum(idx) > 0) {
    params <- rule_params[[sp]]
    crops_all$simplified_pred[idx] <- apply_simplified_rule(
      crops_all$dbh[idx],
      crops_all$cr[idx],
      crops_all$best_log[idx],
      crops_all$site[idx],
      crops_all$drate[idx],
      params
    )
  }
}

# Handle WP (never crop tree) if present
if ("WP" %in% crops_all$spp) {
  crops_all$simplified_pred[crops_all$spp == "WP"] <- FALSE
}

# =============================================================================
# STEP 4: Get original tree predictions for comparison
# =============================================================================

message("Loading original decision trees for comparison...")

crops_all$tree_pred <- NA

for (i in 1:nrow(crops_all)) {
  spp <- crops_all$spp[i]
  site <- crops_all$site[i]
  drate <- crops_all$drate[i]

  # Construct filename
  fname <- sprintf("dt_%s_%s_%dpct.rds", spp, site, round(drate * 100))
  fpath <- file.path(tree_dir, fname)

  if (file.exists(fpath)) {
    tree <- readRDS(fpath)
    if (inherits(tree, "rpart")) {
      pred <- predict(tree, crops_all[i, ], type = "class")
      crops_all$tree_pred[i] <- as.character(pred) == "TRUE"
    } else if (is.list(tree) && "note" %in% names(tree)) {
      if (grepl("No crop", tree$note)) {
        crops_all$tree_pred[i] <- FALSE
      } else if (grepl("All trees are crop", tree$note)) {
        crops_all$tree_pred[i] <- TRUE
      }
    }
  }
}

# =============================================================================
# STEP 5: Calculate accuracy metrics
# =============================================================================

message("\n=== Calculating accuracy metrics ===\n")

calc_metrics <- function(actual, predicted) {
  valid <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid]
  predicted <- predicted[valid]

  if (length(actual) == 0) {
    return(c(n = 0, accuracy = NA, precision = NA, recall = NA,
             specificity = NA, f1 = NA, fpr = NA, fnr = NA))
  }

  tp <- sum(predicted & actual)
  fp <- sum(predicted & !actual)
  tn <- sum(!predicted & !actual)
  fn <- sum(!predicted & actual)
  n <- tp + fp + tn + fn

  accuracy <- (tp + tn) / n
  precision <- ifelse(tp + fp > 0, tp / (tp + fp), NA)
  recall <- ifelse(tp + fn > 0, tp / (tp + fn), NA)
  specificity <- ifelse(tn + fp > 0, tn / (tn + fp), NA)
  f1 <- ifelse(!is.na(precision) & !is.na(recall) & (precision + recall) > 0,
               2 * precision * recall / (precision + recall), NA)
  fpr <- ifelse(fp + tn > 0, fp / (fp + tn), NA)
  fnr <- ifelse(fn + tp > 0, fn / (fn + tp), NA)

  c(n = n, accuracy = accuracy, precision = precision, recall = recall,
    specificity = specificity, f1 = f1, fpr = fpr, fnr = fnr)
}

# Calculate metrics by species (exclude WP if present)
active_species <- unique(crops_all$spp[crops_all$spp != "WP"])

results_by_species <- crops_all %>%
  filter(spp %in% active_species) %>%
  group_by(spp) %>%
  summarise(
    n = n(),
    simp_accuracy = calc_metrics(crop, simplified_pred)["accuracy"],
    simp_precision = calc_metrics(crop, simplified_pred)["precision"],
    simp_recall = calc_metrics(crop, simplified_pred)["recall"],
    simp_f1 = calc_metrics(crop, simplified_pred)["f1"],
    simp_fpr = calc_metrics(crop, simplified_pred)["fpr"],
    simp_fnr = calc_metrics(crop, simplified_pred)["fnr"],
    tree_accuracy = calc_metrics(crop, tree_pred)["accuracy"],
    tree_precision = calc_metrics(crop, tree_pred)["precision"],
    tree_recall = calc_metrics(crop, tree_pred)["recall"],
    tree_f1 = calc_metrics(crop, tree_pred)["f1"],
    tree_fpr = calc_metrics(crop, tree_pred)["fpr"],
    tree_fnr = calc_metrics(crop, tree_pred)["fnr"],
    agreement = mean(simplified_pred == tree_pred, na.rm = TRUE),
    .groups = "drop"
  )

# Print results
message("\n", paste(rep("=", 70), collapse = ""))
message("SIMPLIFIED RULE ACCURACY BY SPECIES")
message(paste(rep("=", 70), collapse = ""), "\n")

cat("\n--- Simplified Rule Performance ---\n")
print(results_by_species %>%
        select(spp, n, simp_accuracy, simp_precision, simp_recall, simp_f1, simp_fpr, simp_fnr) %>%
        mutate(across(where(is.numeric) & !n, ~round(., 3))))

cat("\n--- Original Decision Tree Performance ---\n")
print(results_by_species %>%
        select(spp, n, tree_accuracy, tree_precision, tree_recall, tree_f1, tree_fpr, tree_fnr) %>%
        mutate(across(where(is.numeric) & !n, ~round(., 3))))

cat("\n--- Comparison (Simplified vs Original Tree) ---\n")
comparison <- results_by_species %>%
  select(spp, n, agreement,
         simp_accuracy, tree_accuracy,
         simp_f1, tree_f1) %>%
  mutate(
    accuracy_diff = simp_accuracy - tree_accuracy,
    f1_diff = simp_f1 - tree_f1
  ) %>%
  mutate(across(where(is.numeric) & !n, ~round(., 3)))
print(comparison)

# Overall metrics
message("\n--- Overall Summary ---\n")
overall_simp <- calc_metrics(crops_all$crop[crops_all$spp %in% active_species],
                              crops_all$simplified_pred[crops_all$spp %in% active_species])
overall_tree <- calc_metrics(crops_all$crop[crops_all$spp %in% active_species],
                              crops_all$tree_pred[crops_all$spp %in% active_species])

cat("Simplified Rules (all species):\n")
cat(sprintf("  Accuracy: %.1f%%\n", overall_simp["accuracy"] * 100))
cat(sprintf("  Precision: %.1f%%\n", overall_simp["precision"] * 100))
cat(sprintf("  Recall: %.1f%%\n", overall_simp["recall"] * 100))
cat(sprintf("  F1 Score: %.3f\n", overall_simp["f1"]))
cat(sprintf("  False Positive Rate: %.1f%%\n", overall_simp["fpr"] * 100))
cat(sprintf("  False Negative Rate: %.1f%%\n", overall_simp["fnr"] * 100))

cat("\nOriginal Decision Trees (all species):\n")
cat(sprintf("  Accuracy: %.1f%%\n", overall_tree["accuracy"] * 100))
cat(sprintf("  Precision: %.1f%%\n", overall_tree["precision"] * 100))
cat(sprintf("  Recall: %.1f%%\n", overall_tree["recall"] * 100))
cat(sprintf("  F1 Score: %.3f\n", overall_tree["f1"]))
cat(sprintf("  False Positive Rate: %.1f%%\n", overall_tree["fpr"] * 100))
cat(sprintf("  False Negative Rate: %.1f%%\n", overall_tree["fnr"] * 100))

# Save results
output_file <- file.path(tree_dir, "simplified_rule_accuracy.csv")
write.csv(results_by_species, output_file, row.names = FALSE)
message("\nResults saved to: ", output_file)

# =============================================================================
# STEP 6: Detailed breakdown
# =============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("DETAILED BREAKDOWN BY SPECIES")
message(paste(rep("=", 70), collapse = ""), "\n")

for (spp_code in active_species) {
  cat("\n### ", spp_code, " ###\n")

  spp_data <- crops_all %>% filter(spp == spp_code)

  # By site
  by_site <- spp_data %>%
    group_by(site) %>%
    summarise(
      n = n(),
      simp_acc = mean(simplified_pred == crop, na.rm = TRUE),
      tree_acc = mean(tree_pred == crop, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric) & !n, ~round(., 3)))

  cat("\nBy Site:\n")
  print(by_site)

  # By discount rate
  by_drate <- spp_data %>%
    group_by(drate) %>%
    summarise(
      n = n(),
      simp_acc = mean(simplified_pred == crop, na.rm = TRUE),
      tree_acc = mean(tree_pred == crop, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric) & !n, ~round(., 3)))

  cat("\nBy Discount Rate:\n")
  print(by_drate)
}
