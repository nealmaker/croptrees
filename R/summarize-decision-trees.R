# summarize-decision-trees.R
#
# This script reads decision tree .rds files from a specified directory and
# summarizes them into concise, field-usable rules for foresters.
#
# Usage:
#   source("R/summarize-decision-trees.R")
#   results <- summarize_crop_tree_rules("dataset2")
#
# The script will:
#   1. Extract rules from all decision trees
#   2. Identify patterns across species, sites, and discount rates
#   3. Generate prose summaries with accuracy metrics

library(rpart)
library(dplyr)
library(tidyr)

# =============================================================================
# PART 1: RULE EXTRACTION
# =============================================================================

#' Extract rules from a single rpart decision tree
#'
#' @param tree_fit An rpart object or a list with status info for edge cases
#' @return A data frame with one row per "crop tree" rule, containing variable
#'         thresholds and the node's accuracy metrics
extract_rules_from_tree <- function(tree_fit) {


  # Handle edge cases (no crop trees, all crop trees, fitting failed)
  if (!inherits(tree_fit, "rpart")) {
    if (is.list(tree_fit) && "note" %in% names(tree_fit)) {
      # Return a single-row data frame indicating the edge case
      if (grepl("No crop", tree_fit$note)) {
        return(data.frame(
          rule_id = 1,
          status = "no_crop_trees",
          dbh_min = NA_real_, dbh_max = NA_real_,
          cr_min = NA_real_, cr_max = NA_real_,
          best_log_min = NA_real_, best_log_max = NA_real_,
          n_total = NA_integer_, n_crop = NA_integer_, n_not_crop = NA_integer_,
          node_accuracy = NA_real_,
          stringsAsFactors = FALSE
        ))
      } else if (grepl("All trees are crop", tree_fit$note)) {
        return(data.frame(
          rule_id = 1,
          status = "all_crop_trees",
          dbh_min = -Inf, dbh_max = Inf,
          cr_min = -Inf, cr_max = Inf,
          best_log_min = -Inf, best_log_max = Inf,
          n_total = NA_integer_, n_crop = NA_integer_, n_not_crop = NA_integer_,
          node_accuracy = 1.0,
          stringsAsFactors = FALSE
        ))
      } else {
        return(data.frame(
          rule_id = 1,
          status = "error",
          dbh_min = NA_real_, dbh_max = NA_real_,
          cr_min = NA_real_, cr_max = NA_real_,
          best_log_min = NA_real_, best_log_max = NA_real_,
          n_total = NA_integer_, n_crop = NA_integer_, n_not_crop = NA_integer_,
          node_accuracy = NA_real_,
          stringsAsFactors = FALSE
        ))
      }
    }
    return(NULL)
  }

  # Get the frame with node information

  frame <- tree_fit$frame

  # Find terminal nodes (leaves)
  terminal_idx <- which(frame$var == "<leaf>")

  # Identify which class is "crop" (TRUE)
  response_levels <- attr(tree_fit, "ylevels")
  if (is.null(response_levels)) {
    crop_class <- 2  # Default for logical
  } else {
    crop_class <- which(response_levels %in% c("TRUE", "1", "Yes"))
    if (length(crop_class) == 0) crop_class <- 2
  }

  # Find terminal nodes that predict CROP
  crop_terminal_idx <- terminal_idx[frame$yval[terminal_idx] == crop_class]

  if (length(crop_terminal_idx) == 0) {
    return(data.frame(
      rule_id = 1,
      status = "no_crop_trees",
      dbh_min = NA_real_, dbh_max = NA_real_,
      cr_min = NA_real_, cr_max = NA_real_,
      best_log_min = NA_real_, best_log_max = NA_real_,
      n_total = NA_integer_, n_crop = NA_integer_, n_not_crop = NA_integer_,
      node_accuracy = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  # Get paths to crop nodes
  crop_node_nums <- as.numeric(rownames(frame)[crop_terminal_idx])
  all_paths <- path.rpart(tree_fit, nodes = crop_node_nums, print.it = FALSE)

  # Extract rules for each crop node
  rules_list <- lapply(seq_along(crop_node_nums), function(i) {
    node_num <- crop_node_nums[i]
    node_idx <- crop_terminal_idx[i]
    path_text <- all_paths[[as.character(node_num)]]

    # Initialize variable ranges
    dbh_min <- -Inf; dbh_max <- Inf
    cr_min <- -Inf; cr_max <- Inf
    best_log_min <- -Inf; best_log_max <- Inf

    # Parse path conditions
    if (length(path_text) > 1) {
      conditions <- path_text[-1]  # Skip "root"

      for (cond in conditions) {
        cond <- gsub(" ", "", cond)  # Remove spaces

        # Parse the condition
        if (grepl("dbh", cond)) {
          if (grepl(">=", cond)) {
            val <- as.numeric(sub(".*>=", "", cond))
            dbh_min <- max(dbh_min, val)
          } else if (grepl("<", cond)) {
            val <- as.numeric(sub(".*<", "", cond))
            dbh_max <- min(dbh_max, val)
          }
        } else if (grepl("cr", cond)) {
          if (grepl(">=", cond)) {
            val <- as.numeric(sub(".*>=", "", cond))
            cr_min <- max(cr_min, val)
          } else if (grepl("<", cond)) {
            val <- as.numeric(sub(".*<", "", cond))
            cr_max <- min(cr_max, val)
          }
        } else if (grepl("best_log", cond)) {
          if (grepl(">=", cond)) {
            val <- as.numeric(sub(".*>=", "", cond))
            best_log_min <- max(best_log_min, val)
          } else if (grepl("<", cond)) {
            val <- as.numeric(sub(".*<", "", cond))
            best_log_max <- min(best_log_max, val)
          }
        }
      }
    }

    # Get node counts from yval2 if available, otherwise from n
    n_total <- frame$n[node_idx]
    if (!is.null(frame$yval2)) {
      # yval2 contains class counts: [predicted_class, n_class1, n_class2, prob1, prob2]
      yval2 <- frame$yval2[node_idx, ]
      if (crop_class == 1) {
        n_crop <- yval2[2]
        n_not_crop <- yval2[3]
      } else {
        n_crop <- yval2[3]
        n_not_crop <- yval2[2]
      }
      node_accuracy <- n_crop / n_total
    } else {
      n_crop <- NA_integer_
      n_not_crop <- NA_integer_
      node_accuracy <- NA_real_
    }

    data.frame(
      rule_id = i,
      status = "valid",
      dbh_min = dbh_min,
      dbh_max = dbh_max,
      cr_min = cr_min,
      cr_max = cr_max,
      best_log_min = best_log_min,
      best_log_max = best_log_max,
      n_total = as.integer(n_total),
      n_crop = as.integer(n_crop),
      n_not_crop = as.integer(n_not_crop),
      node_accuracy = node_accuracy,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rules_list)
}


#' Read all decision trees from a directory and extract rules
#'
#' @param outdir Name of the subdirectory within output/decision-trees/
#' @param base_path Base path to the project (default: current directory)
#' @return A data frame with all rules, including spp, site, drate columns
extract_all_rules <- function(outdir = "dataset2",
                               base_path = ".") {


  # Build path to decision tree directory
  dt_path <- file.path(base_path, "output", "decision-trees", outdir)

  if (!dir.exists(dt_path)) {
    stop("Directory not found: ", dt_path)
  }

  # Find all .rds files
  rds_files <- list.files(dt_path, pattern = "\\.rds$", full.names = TRUE)

  if (length(rds_files) == 0) {
    stop("No .rds files found in: ", dt_path)
  }

  message("Found ", length(rds_files), " decision tree files")

  # Parse filenames to extract metadata
  # Expected format: dt_SPP_site_Xpct.rds
  all_rules <- lapply(rds_files, function(f) {
    fname <- basename(f)

    # Parse filename
    parts <- strsplit(gsub("\\.rds$", "", fname), "_")[[1]]
    if (length(parts) < 4) {
      warning("Unexpected filename format: ", fname)
      return(NULL)
    }

    spp <- parts[2]
    site <- parts[3]
    drate_str <- parts[4]
    drate <- as.numeric(gsub("pct", "", drate_str)) / 100

    # Read the tree
    tree <- tryCatch(
      readRDS(f),
      error = function(e) {
        warning("Error reading ", fname, ": ", e$message)
        return(NULL)
      }
    )

    if (is.null(tree)) return(NULL)

    # Extract rules
    rules <- extract_rules_from_tree(tree)

    if (is.null(rules) || nrow(rules) == 0) return(NULL)

    # Add metadata
    rules$spp <- spp
    rules$site <- site
    rules$drate <- drate
    rules$filename <- fname

    rules
  })

  # Combine all rules
  all_rules_df <- do.call(rbind, Filter(Negate(is.null), all_rules))

  # Reorder columns
  col_order <- c("spp", "site", "drate", "rule_id", "status",
                 "dbh_min", "dbh_max", "cr_min", "cr_max",
                 "best_log_min", "best_log_max",
                 "n_total", "n_crop", "n_not_crop", "node_accuracy", "filename")
  all_rules_df <- all_rules_df[, col_order]

  message("Extracted ", nrow(all_rules_df), " rules from ",
          length(unique(all_rules_df$filename)), " trees")

  all_rules_df
}


# =============================================================================
# PART 2: RULE CLASSIFICATION AND PATTERN ANALYSIS
# =============================================================================

#' Classify the logical form of rules for a species
#'
#' @param rules_df Data frame of rules for a single species (must have spp, site, drate columns)
#' @return Character string describing the dominant logical form
classify_rule_form <- function(rules_df) {

  # Filter to valid rules only
  valid_rules <- rules_df[rules_df$status == "valid", ]

  if (nrow(valid_rules) == 0) {
    if (all(rules_df$status == "no_crop_trees")) {
      return("never_crop")
    } else if (all(rules_df$status == "all_crop_trees")) {
      return("always_crop")
    }
    return("unknown")
  }

  # Check which variables are used (have finite bounds)
  uses_dbh <- any(is.finite(valid_rules$dbh_min) | is.finite(valid_rules$dbh_max))
  uses_cr <- any(is.finite(valid_rules$cr_min) | is.finite(valid_rules$cr_max))
  uses_grade <- any(is.finite(valid_rules$best_log_min) | is.finite(valid_rules$best_log_max))

  # Count how many rules per tree (indicates complexity)
  # Check if grouping columns exist
  if (all(c("site", "drate") %in% names(valid_rules))) {
    rules_per_tree <- valid_rules %>%
      group_by(site, drate) %>%
      summarise(n_rules = n(), .groups = "drop")
    avg_rules <- mean(rules_per_tree$n_rules)
  } else {
    # If no grouping columns, count total rules
    avg_rules <- nrow(valid_rules)
  }

  # Classify based on which variables are used and complexity
  if (!uses_dbh && !uses_cr && !uses_grade) {
    return("always_crop")
  }

  if (uses_dbh && !uses_cr && !uses_grade) {
    return("dbh_only")
  }

  if (uses_dbh && uses_cr && !uses_grade) {
    # Check if CR creates different DBH thresholds (CR-dependent form)
    if (avg_rules > 1.2) {
      return("cr_dependent")
    }
    return("simple_threshold")
  }

  if (uses_dbh && uses_grade && !uses_cr) {
    return("grade_dependent")
  }

  if (uses_dbh && uses_cr && uses_grade) {
    if (avg_rules > 1.5) {
      return("mixed_complex")
    }
    return("mixed_simple")
  }

  return("other")
}


#' Simplify rules to key thresholds for summarization
#'
#' For each species/site/drate combination, extract the effective thresholds
#'
#' @param rules_df Data frame of all rules
#' @return Data frame with simplified thresholds
simplify_rules <- function(rules_df) {

  # Helper function to safely compute max/min on potentially empty vectors
  safe_max <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_real_)
    max(x, na.rm = TRUE)
  }

  safe_min <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_real_)
    min(x, na.rm = TRUE)
  }

  # Group by species, site, drate and summarize
  simplified <- rules_df %>%
    filter(status == "valid") %>%
    group_by(spp, site, drate) %>%
    summarise(
      n_rules = n(),
      # For DBH, take the maximum of dbh_max across rules (most permissive upper bound)
      dbh_max_overall = safe_max(dbh_max),
      # Minimum DBH requirement (if any)
      dbh_min_overall = safe_min(dbh_min),
      # For CR, take the minimum of cr_min (least restrictive lower bound)
      cr_min_overall = safe_min(cr_min),
      # Check if grade matters
      grade_matters = any(is.finite(best_log_min) | is.finite(best_log_max)),
      # If grade matters, what's the threshold for veneer (best_log <= 1)?
      veneer_dbh_max = {
        idx <- which(best_log_max < 1.5 & is.finite(best_log_max))
        if (length(idx) > 0) safe_max(dbh_max[idx]) else NA_real_
      },
      # Sawlog threshold (best_log >= 2)?
      sawlog_dbh_max = {
        idx <- which(best_log_min >= 1.5 & is.finite(best_log_min))
        if (length(idx) > 0) safe_max(dbh_max[idx]) else NA_real_
      },
      # Check if CR creates different DBH limits - high CR
      high_cr_dbh_max = {
        idx <- which(cr_min >= 25 & is.finite(cr_min))
        if (length(idx) > 0) safe_max(dbh_max[idx]) else NA_real_
      },
      # Low CR threshold
      low_cr_dbh_max = {
        idx <- which(cr_max < 35 & is.finite(cr_max))
        if (length(idx) > 0) safe_max(dbh_max[idx]) else NA_real_
      },
      # Total observations and accuracy
      total_n = sum(n_total, na.rm = TRUE),
      total_crop = sum(n_crop, na.rm = TRUE),
      weighted_accuracy = sum(n_crop, na.rm = TRUE) / sum(n_total, na.rm = TRUE),
      .groups = "drop"
    )

  # Handle infinite values
  simplified <- simplified %>%
    mutate(
      dbh_max_overall = ifelse(is.infinite(dbh_max_overall), NA_real_, dbh_max_overall),
      dbh_min_overall = ifelse(is.infinite(dbh_min_overall), NA_real_, dbh_min_overall),
      cr_min_overall = ifelse(is.infinite(cr_min_overall), NA_real_, cr_min_overall)
    )

  simplified
}


#' Analyze how thresholds change with discount rate
#'
#' @param simplified_df Output from simplify_rules()
#' @return Data frame with discount rate effects by species and site
analyze_drate_effects <- function(simplified_df) {


  # For each species-site combination, fit a simple linear model
  # of DBH threshold vs discount rate
  drate_effects <- simplified_df %>%
    filter(!is.na(dbh_max_overall)) %>%
    group_by(spp, site) %>%
    summarise(
      n_rates = n(),
      dbh_at_3pct = dbh_max_overall[which.min(abs(drate - 0.03))],
      cr_at_3pct = cr_min_overall[which.min(abs(drate - 0.03))],
      # Fit linear model for DBH vs drate
      dbh_slope = ifelse(
        n() >= 3,
        coef(lm(dbh_max_overall ~ drate))[2],
        NA_real_
      ),
      dbh_intercept = ifelse(
        n() >= 3,
        coef(lm(dbh_max_overall ~ drate))[1],
        NA_real_
      ),
      # Fit linear model for CR vs drate
      cr_slope = ifelse(
        n() >= 3 && sum(!is.na(cr_min_overall)) >= 3,
        coef(lm(cr_min_overall ~ drate, na.action = na.omit))[2],
        NA_real_
      ),
      .groups = "drop"
    )

  # Convert slope to "per 1% change"
  drate_effects <- drate_effects %>%
    mutate(
      dbh_change_per_pct = dbh_slope / 100,  # Already in decimal, convert to per-percent
      cr_change_per_pct = cr_slope / 100
    )

  drate_effects
}


#' Analyze site effects on thresholds
#'
#' @param simplified_df Output from simplify_rules()
#' @return Data frame with site effects by species
analyze_site_effects <- function(simplified_df) {


  # Use 3% discount rate as reference
  ref_data <- simplified_df %>%
    filter(abs(drate - 0.03) < 0.001)

  if (nrow(ref_data) == 0) {
    # Try closest available rate
    closest_rate <- simplified_df$drate[which.min(abs(simplified_df$drate - 0.03))]
    ref_data <- simplified_df %>%
      filter(abs(drate - closest_rate) < 0.001)
  }

  # Calculate site effects relative to mean for each species
  site_effects <- ref_data %>%
    group_by(spp) %>%
    mutate(
      mean_dbh = mean(dbh_max_overall, na.rm = TRUE),
      mean_cr = mean(cr_min_overall, na.rm = TRUE),
      dbh_site_effect = dbh_max_overall - mean_dbh,
      cr_site_effect = cr_min_overall - mean_cr
    ) %>%
    ungroup() %>%
    select(spp, site, dbh_max_overall, cr_min_overall,
           mean_dbh, mean_cr, dbh_site_effect, cr_site_effect,
           grade_matters, veneer_dbh_max, sawlog_dbh_max,
           high_cr_dbh_max, low_cr_dbh_max)

  site_effects
}


# =============================================================================
# PART 3: ACCURACY ASSESSMENT
# =============================================================================

#' Calculate accuracy metrics for original decision trees
#'
#' This extracts confusion matrix info from the trees themselves
#'
#' @param rules_df Data frame of all rules from extract_all_rules()
#' @return Data frame with accuracy metrics by species/site/drate
calculate_tree_accuracy <- function(rules_df) {


  # For each tree, calculate overall accuracy metrics
  accuracy_df <- rules_df %>%
    filter(status == "valid") %>%
    group_by(spp, site, drate) %>%
    summarise(
      # Sum up all observations in crop-predicting nodes
      n_predicted_crop = sum(n_total, na.rm = TRUE),
      n_true_positive = sum(n_crop, na.rm = TRUE),
      n_false_positive = sum(n_not_crop, na.rm = TRUE),
      .groups = "drop"
    )

  accuracy_df
}


#' Evaluate simplified rules against original trees
#'
#' This function takes simplified prose rules and evaluates how well they
#' approximate the original decision tree predictions.
#'
#' @param simplified_rules List of simplified rule specifications
#' @param original_data The original data used to fit trees (if available)
#' @param trees_dir Directory containing the original tree files
#' @return Data frame with accuracy comparison
evaluate_simplified_rules <- function(simplified_rules, original_data = NULL,
                                       trees_dir = "output/decision-trees/dataset2") {


  message("Accuracy evaluation requires the original training data.")
  message("If available, pass it via the 'original_data' parameter.")
  message("Otherwise, we report the within-node accuracy from the trees.")

  # For now, return the tree-based accuracy
  return(NULL)
}


#' Calculate accuracy metrics for a single tree vs its simplified rule
#'
#' @param tree_fit The original rpart tree
#' @param simplified_rule A function that takes dbh, cr, best_log and returns TRUE/FALSE
#' @param data Original data (must have dbh, cr, best_log, crop columns)
#' @return Named vector with accuracy metrics
compare_tree_to_rule <- function(tree_fit, simplified_rule, data) {


  if (!inherits(tree_fit, "rpart") || is.null(data)) {
    return(c(accuracy = NA, sensitivity = NA, specificity = NA,
             ppv = NA, npv = NA, fpr = NA, fnr = NA))
  }

  # Get tree predictions
  tree_pred <- predict(tree_fit, data, type = "class")
  tree_pred_logical <- as.logical(tree_pred == "TRUE" | tree_pred == "1")

  # Get simplified rule predictions
  rule_pred <- simplified_rule(data$dbh, data$cr, data$best_log)

  # Calculate agreement between tree and simplified rule
  agreement <- mean(tree_pred_logical == rule_pred, na.rm = TRUE)

  # Also calculate vs actual outcomes
  actual <- as.logical(data$crop)

  # Tree metrics
  tree_tp <- sum(tree_pred_logical & actual, na.rm = TRUE)
  tree_fp <- sum(tree_pred_logical & !actual, na.rm = TRUE)
  tree_tn <- sum(!tree_pred_logical & !actual, na.rm = TRUE)
  tree_fn <- sum(!tree_pred_logical & actual, na.rm = TRUE)

  # Simplified rule metrics
  rule_tp <- sum(rule_pred & actual, na.rm = TRUE)
  rule_fp <- sum(rule_pred & !actual, na.rm = TRUE)
  rule_tn <- sum(!rule_pred & !actual, na.rm = TRUE)
  rule_fn <- sum(!rule_pred & actual, na.rm = TRUE)

  c(
    tree_rule_agreement = agreement,
    tree_accuracy = (tree_tp + tree_tn) / length(actual),
    tree_sensitivity = tree_tp / (tree_tp + tree_fn),
    tree_specificity = tree_tn / (tree_tn + tree_fp),
    tree_fpr = tree_fp / (tree_fp + tree_tn),
    tree_fnr = tree_fn / (tree_fn + tree_tp),
    rule_accuracy = (rule_tp + rule_tn) / length(actual),
    rule_sensitivity = rule_tp / (rule_tp + rule_fn),
    rule_specificity = rule_tn / (rule_tn + rule_fp),
    rule_fpr = rule_fp / (rule_fp + rule_tn),
    rule_fnr = rule_fn / (rule_fn + rule_tp)
  )
}


# =============================================================================
# PART 4: PROSE SUMMARY GENERATION
# =============================================================================

#' Analyze grade effects from raw rules for a species at 3% discount rate
#'
#' @param raw_rules Raw rules data frame
#' @param spp Species code
#' @return List with grade analysis results
analyze_grade_effects <- function(raw_rules, spp) {

  # Filter to 3% rules for this species
  spp_rules <- raw_rules %>%
    filter(spp == !!spp, status == "valid", abs(drate - 0.03) < 0.005)

  if (nrow(spp_rules) == 0) {
    # Try other rates
    spp_rules <- raw_rules %>%
      filter(spp == !!spp, status == "valid")
  }

  if (nrow(spp_rules) == 0) {
    return(list(grade_matters = FALSE))
  }

  # Check if any rules have grade constraints
  # In rpart splits: best_log_max <= 1.5 captures veneer (best_log = 1)
  # best_log_min >= 1.5 captures sawlog (best_log = 2)
  has_veneer_constraint <- any(spp_rules$best_log_max <= 1.5 & is.finite(spp_rules$best_log_max))
  has_sawlog_constraint <- any(spp_rules$best_log_min >= 1.5 & is.finite(spp_rules$best_log_min))

  grade_matters <- has_veneer_constraint || has_sawlog_constraint

  if (!grade_matters) {
    return(list(grade_matters = FALSE))
  }

  # Analyze veneer-specific rules (best_log = 1, meaning best_log_max <= 1.5)
  veneer_rules <- spp_rules %>%
    filter(best_log_max <= 1.5 & is.finite(best_log_max))

  # Analyze sawlog-specific rules (best_log = 2, meaning best_log_min >= 1.5)
  sawlog_rules <- spp_rules %>%
    filter(best_log_min >= 1.5 & is.finite(best_log_min))

  # Analyze grade-agnostic rules (no grade constraint)
  any_grade_rules <- spp_rules %>%
    filter(!is.finite(best_log_min) & !is.finite(best_log_max))

  # Calculate DBH limits for each grade category
  veneer_dbh_max <- if (nrow(veneer_rules) > 0) {
    max(veneer_rules$dbh_max[is.finite(veneer_rules$dbh_max)], na.rm = TRUE)
  } else NA_real_

  sawlog_dbh_max <- if (nrow(sawlog_rules) > 0) {
    max(sawlog_rules$dbh_max[is.finite(sawlog_rules$dbh_max)], na.rm = TRUE)
  } else NA_real_

  any_grade_dbh_max <- if (nrow(any_grade_rules) > 0) {
    max(any_grade_rules$dbh_max[is.finite(any_grade_rules$dbh_max)], na.rm = TRUE)
  } else NA_real_

  # Handle infinite results
  if (is.infinite(veneer_dbh_max)) veneer_dbh_max <- NA_real_
  if (is.infinite(sawlog_dbh_max)) sawlog_dbh_max <- NA_real_
  if (is.infinite(any_grade_dbh_max)) any_grade_dbh_max <- NA_real_

  # Determine the pattern
  # Pattern 1: Veneer allows higher DBH than sawlog
  # Pattern 2: Only veneer trees can be crops (sawlog never crops)
  # Pattern 3: Grade extends the DBH limit beyond the general rule

  veneer_advantage <- 0
  if (!is.na(veneer_dbh_max) && !is.na(sawlog_dbh_max)) {
    veneer_advantage <- veneer_dbh_max - sawlog_dbh_max
  } else if (!is.na(veneer_dbh_max) && !is.na(any_grade_dbh_max)) {
    veneer_advantage <- veneer_dbh_max - any_grade_dbh_max
  }

  # Check if veneer-only at higher DBH (sawlog must be smaller)
  veneer_only_high_dbh <- !is.na(veneer_dbh_max) &&
    (is.na(sawlog_dbh_max) || veneer_dbh_max > sawlog_dbh_max) &&
    (!is.na(any_grade_dbh_max) && veneer_dbh_max > any_grade_dbh_max)

  list(
    grade_matters = TRUE,
    veneer_dbh_max = veneer_dbh_max,
    sawlog_dbh_max = sawlog_dbh_max,
    any_grade_dbh_max = any_grade_dbh_max,
    veneer_advantage = veneer_advantage,
    veneer_only_high_dbh = veneer_only_high_dbh,
    n_veneer_rules = nrow(veneer_rules),
    n_sawlog_rules = nrow(sawlog_rules),
    n_any_grade_rules = nrow(any_grade_rules)
  )
}


#' Analyze CR effects from raw rules for a species
#'
#' @param raw_rules Raw rules data frame
#' @param spp Species code
#' @return List with CR analysis results
analyze_cr_effects <- function(raw_rules, spp) {

  # Filter to 3% rules for this species
  spp_rules <- raw_rules %>%
    filter(spp == !!spp, status == "valid", abs(drate - 0.03) < 0.005)

  if (nrow(spp_rules) == 0) {
    spp_rules <- raw_rules %>%
      filter(spp == !!spp, status == "valid")
  }

  if (nrow(spp_rules) == 0) {
    return(list(cr_matters = FALSE))
  }

  # Check if CR creates different DBH thresholds
  # Look for rules with different cr_min or cr_max values
  has_cr_min <- any(is.finite(spp_rules$cr_min))
  has_cr_max <- any(is.finite(spp_rules$cr_max))

  if (!has_cr_min && !has_cr_max) {
    return(list(cr_matters = FALSE, min_cr = NA_real_))
  }

  # Find the minimum CR requirement across all rules
  min_cr_required <- min(spp_rules$cr_min[is.finite(spp_rules$cr_min)], na.rm = TRUE)
  if (is.infinite(min_cr_required)) min_cr_required <- NA_real_

  # Check if there are rules with cr_max constraints (low CR rules)
  low_cr_rules <- spp_rules %>%
    filter(is.finite(cr_max))

  high_cr_rules <- spp_rules %>%
    filter(is.finite(cr_min) & cr_min >= 25)

  # Check if high CR allows higher DBH
  low_cr_dbh_max <- if (nrow(low_cr_rules) > 0) {
    max(low_cr_rules$dbh_max[is.finite(low_cr_rules$dbh_max)], na.rm = TRUE)
  } else NA_real_

  high_cr_dbh_max <- if (nrow(high_cr_rules) > 0) {
    max(high_cr_rules$dbh_max[is.finite(high_cr_rules$dbh_max)], na.rm = TRUE)
  } else NA_real_

  if (is.infinite(low_cr_dbh_max)) low_cr_dbh_max <- NA_real_
  if (is.infinite(high_cr_dbh_max)) high_cr_dbh_max <- NA_real_

  # Determine if there's a CR-dependent pattern
  cr_dependent <- !is.na(low_cr_dbh_max) && !is.na(high_cr_dbh_max) &&
    abs(high_cr_dbh_max - low_cr_dbh_max) >= 1

  # Find the CR threshold used for splits
  cr_thresholds <- unique(c(spp_rules$cr_min[is.finite(spp_rules$cr_min)],
                            spp_rules$cr_max[is.finite(spp_rules$cr_max)]))
  typical_cr_threshold <- if (length(cr_thresholds) > 0) {
    median(cr_thresholds)
  } else NA_real_

  list(
    cr_matters = TRUE,
    cr_dependent = cr_dependent,
    min_cr_required = min_cr_required,
    typical_cr_threshold = typical_cr_threshold,
    low_cr_dbh_max = low_cr_dbh_max,
    high_cr_dbh_max = high_cr_dbh_max
  )
}


#' Generate a prose summary for a single species
#'
#' @param spp Species code
#' @param simplified_df Simplified rules data frame
#' @param site_effects Site effects data frame
#' @param drate_effects Discount rate effects data frame
#' @param raw_rules Raw rules data frame (optional, for detailed analysis)
#' @return Character string with the prose summary
generate_species_summary <- function(spp, simplified_df, site_effects, drate_effects,
                                      raw_rules = NULL) {

  spp_data <- simplified_df %>% filter(spp == !!spp)
  spp_site_fx <- site_effects %>% filter(spp == !!spp)
  spp_drate_fx <- drate_effects %>% filter(spp == !!spp)

  if (nrow(spp_data) == 0) {
    return(paste0(spp, ": No valid rules found."))
  }

  # Build the summary
  lines <- character()
  lines <- c(lines, paste0("## ", spp))

  # Base rule description
  if (all(is.na(spp_data$dbh_max_overall))) {
    lines <- c(lines, "Never a crop tree (harvest immediately).")
    return(paste(lines, collapse = "\n"))
  }

  # Analyze grade and CR effects from raw rules
  grade_info <- if (!is.null(raw_rules)) {
    analyze_grade_effects(raw_rules, spp)
  } else {
    list(grade_matters = FALSE)
  }

  cr_info <- if (!is.null(raw_rules)) {
    analyze_cr_effects(raw_rules, spp)
  } else {
    list(cr_matters = FALSE, cr_dependent = FALSE)
  }

  # Get overall DBH at 3%
  ref_data <- spp_data %>% filter(abs(drate - 0.03) < 0.005)
  if (nrow(ref_data) == 0) ref_data <- spp_data
  overall_dbh <- floor(median(ref_data$dbh_max_overall, na.rm = TRUE))

  # Determine the primary pattern and generate appropriate prose
  # Priority: 1) Grade+CR combined, 2) CR-dependent, 3) Grade-dependent, 4) Simple

  if (grade_info$grade_matters && cr_info$cr_dependent) {
    # Complex case: both grade and CR matter
    lines <- c(lines, "**Base rule (3% discount rate):**")

    # Describe the CR-dependent thresholds first (use floor for DBH upper bounds)
    high_cr_dbh <- floor(cr_info$high_cr_dbh_max)
    low_cr_dbh <- floor(cr_info$low_cr_dbh_max)
    cr_thresh <- round(cr_info$typical_cr_threshold)

    if (!is.na(high_cr_dbh) && !is.na(low_cr_dbh)) {
      lines <- c(lines, paste0(
        "- With crown ratio >= ", cr_thresh, "%: DBH <= ", high_cr_dbh, "\""
      ))
      lines <- c(lines, paste0(
        "- With crown ratio < ", cr_thresh, "%: DBH <= ", low_cr_dbh, "\""
      ))
    }

    # Add grade modifier if veneer allows higher DBH
    if (!is.na(grade_info$veneer_advantage) && grade_info$veneer_advantage >= 1) {
      lines <- c(lines, paste0(
        "- **Veneer bonus:** Veneer-quality trees can be +",
        floor(grade_info$veneer_advantage), "\" larger"
      ))
    }

  } else if (cr_info$cr_dependent) {
    # CR-dependent pattern (like YB)
    high_cr_dbh <- floor(cr_info$high_cr_dbh_max)
    low_cr_dbh <- floor(cr_info$low_cr_dbh_max)
    cr_thresh <- round(cr_info$typical_cr_threshold)

    lines <- c(lines, paste0(
      "**Base rule (3% discount rate):** Crop tree if DBH <= ", high_cr_dbh,
      "\" when crown ratio >= ", cr_thresh, "%, or DBH <= ", low_cr_dbh,
      "\" when crown ratio < ", cr_thresh, "%"
    ))

    # Note: higher CR allows LARGER trees (more room to grow)
    if (high_cr_dbh > low_cr_dbh) {
      lines <- c(lines, paste0(
        "*Note: Trees with better crowns (higher CR) can be retained at larger sizes.*"
      ))
    }

  } else if (grade_info$grade_matters) {
    # Grade-dependent pattern
    # Use floor() for DBH upper bounds to get conservative values and preserve distinctions
    veneer_dbh <- floor(grade_info$veneer_dbh_max)
    sawlog_dbh <- floor(grade_info$sawlog_dbh_max)
    any_dbh <- floor(grade_info$any_grade_dbh_max)
    min_cr <- round(cr_info$min_cr_required)

    if (!is.na(veneer_dbh) && !is.na(any_dbh) && veneer_dbh > any_dbh) {
      # Veneer extends the DBH limit
      lines <- c(lines, paste0(
        "**Base rule (3% discount rate):** Crop tree if DBH <= ", any_dbh,
        "\" for sawtimber or veneer, or DBH <= ", veneer_dbh,
        "\" if veneer-quality"
      ))
      if (!is.na(min_cr) && min_cr > 0) {
        lines <- c(lines, paste0("Requires crown ratio >= ", min_cr, "%"))
      }
    } else if (!is.na(veneer_dbh) && !is.na(sawlog_dbh)) {
      # Different limits for veneer vs sawlog
      lines <- c(lines, paste0(
        "**Base rule (3% discount rate):** Crop tree if:"
      ))
      lines <- c(lines, paste0(
        "- Veneer-quality: DBH <= ", veneer_dbh, "\""
      ))
      lines <- c(lines, paste0(
        "- Sawlog-quality: DBH <= ", sawlog_dbh, "\""
      ))
      if (!is.na(min_cr) && min_cr > 0) {
        lines <- c(lines, paste0("Requires crown ratio >= ", min_cr, "%"))
      }
    } else if (!is.na(veneer_dbh) && is.na(sawlog_dbh)) {
      # Only veneer trees can be crops
      lines <- c(lines, paste0(
        "**Base rule (3% discount rate):** Crop tree only if veneer-quality ",
        "and DBH <= ", veneer_dbh, "\""
      ))
      if (!is.na(min_cr) && min_cr > 0) {
        lines <- c(lines, paste0("Requires crown ratio >= ", min_cr, "%"))
      }
      lines <- c(lines, "*Sawlog-quality trees are not crop trees.*")
    } else {
      # Fallback
      lines <- c(lines, paste0(
        "**Base rule (3% discount rate):** Crop tree if DBH <= ", overall_dbh, "\""
      ))
      if (!is.na(min_cr) && min_cr > 0) {
        lines <- c(lines, paste0("Requires crown ratio >= ", min_cr, "%"))
      }
      lines <- c(lines, "*Log grade affects eligibility in some conditions.*")
    }

  } else {
    # Simple threshold form
    min_cr <- round(cr_info$min_cr_required)

    lines <- c(lines, paste0(
      "**Base rule (3% discount rate):** Crop tree if DBH <= ", overall_dbh, "\""
    ))
    if (!is.na(min_cr) && min_cr > 0) {
      lines <- c(lines, paste0("Requires crown ratio >= ", min_cr, "%"))
    }
  }

  # Add discount rate adjustment
  avg_slope <- mean(spp_drate_fx$dbh_slope, na.rm = TRUE)
  if (!is.na(avg_slope) && abs(avg_slope) > 5) {
    dbh_per_pct <- round(avg_slope / 100, 1)
    direction <- ifelse(dbh_per_pct < 0, "decreases", "increases")
    lines <- c(lines, paste0(
      "\n**Discount rate adjustment:** Max DBH ", direction, " by ~",
      abs(dbh_per_pct), "\" for each 1% increase in discount rate"
    ))
  }

  # Add site adjustments
  if (nrow(spp_site_fx) > 1) {
    site_summary <- spp_site_fx %>%
      filter(!is.na(dbh_site_effect)) %>%
      arrange(desc(dbh_site_effect))

    if (nrow(site_summary) > 0 && max(abs(site_summary$dbh_site_effect), na.rm = TRUE) > 0.5) {
      lines <- c(lines, "\n**Site adjustments:**")
      for (i in 1:nrow(site_summary)) {
        effect <- round(site_summary$dbh_site_effect[i], 1)
        site_name <- site_summary$site[i]
        if (abs(effect) > 0.5) {
          direction <- ifelse(effect > 0, "+", "")
          lines <- c(lines, paste0("- ", site_name, ": ", direction, effect, "\" to max DBH"))
        }
      }
    }
  }

  paste(lines, collapse = "\n")
}


#' Generate complete summary report for all species
#'
#' @param rules_df Output from extract_all_rules()
#' @param include_accuracy Whether to include accuracy metrics
#' @return List with prose summaries and data tables
generate_full_report <- function(rules_df, include_accuracy = TRUE) {


  message("Analyzing rules...")

  # Simplify rules
  simplified <- simplify_rules(rules_df)

  # Analyze effects
  site_effects <- analyze_site_effects(simplified)
  drate_effects <- analyze_drate_effects(simplified)

  # Classify each species - use a different approach to avoid cur_data() issues
  species_list_all <- unique(rules_df$spp)
  species_forms <- data.frame(
    spp = species_list_all,
    form = sapply(species_list_all, function(sp) {
      classify_rule_form(rules_df[rules_df$spp == sp, ])
    }),
    stringsAsFactors = FALSE
  )

  message("\nSpecies rule forms:")
  print(species_forms)

  # Generate summaries for each species
  species_list <- unique(rules_df$spp)
  species_list <- species_list[species_list != "WP"]  # Exclude WP as requested

  summaries <- sapply(species_list, function(sp) {
    generate_species_summary(sp, simplified, site_effects, drate_effects, raw_rules = rules_df)
  })

  # Calculate accuracy if requested
  accuracy <- NULL
  if (include_accuracy) {
    accuracy <- calculate_tree_accuracy(rules_df)

    # Add summary statistics
    accuracy <- accuracy %>%
      mutate(
        precision = n_true_positive / (n_true_positive + n_false_positive),
        # Note: recall/sensitivity requires knowing total actual crop trees
        # which we don't have from just the tree structure
        fpr_estimate = n_false_positive / n_predicted_crop
      )
  }

  # Compile results
  list(
    summaries = summaries,
    simplified_rules = simplified,
    site_effects = site_effects,
    drate_effects = drate_effects,
    species_forms = species_forms,
    accuracy = accuracy,
    raw_rules = rules_df
  )
}


# =============================================================================
# PART 5: MAIN FUNCTION AND EXPORT
# =============================================================================

#' Main function to summarize crop tree rules
#'
#' @param outdir Name of subdirectory in output/decision-trees/
#' @param base_path Base path to project
#' @param save_output Whether to save results to files
#' @return List with all analysis results
summarize_crop_tree_rules <- function(outdir = "dataset2",
                                       base_path = ".",
                                       save_output = TRUE) {


  message("=== Crop Tree Rule Summarization ===\n")
  message("Reading decision trees from: ", outdir, "\n")

  # Extract all rules
  rules_df <- extract_all_rules(outdir, base_path)

  # Generate full report
  report <- generate_full_report(rules_df)

  # Print summaries
  message("\n", paste(rep("=", 60), collapse = ""))
  message("SPECIES SUMMARIES")
  message(paste(rep("=", 60), collapse = ""), "\n")

  for (summary in report$summaries) {
    cat(summary, "\n\n")
  }

  # Print accuracy summary if available
  if (!is.null(report$accuracy)) {
    message(paste(rep("=", 60), collapse = ""))
    message("ACCURACY SUMMARY (within-tree metrics)")
    message(paste(rep("=", 60), collapse = ""), "\n")

    acc_summary <- report$accuracy %>%
      group_by(spp) %>%
      summarise(
        avg_precision = mean(precision, na.rm = TRUE),
        min_precision = min(precision, na.rm = TRUE),
        max_precision = max(precision, na.rm = TRUE),
        .groups = "drop"
      )

    print(acc_summary)
  }

  # Save output if requested
  if (save_output) {
    output_path <- file.path(base_path, "output", "decision-trees", outdir)

    # Save simplified rules as CSV
    write.csv(
      report$simplified_rules,
      file.path(output_path, "simplified_rules.csv"),
      row.names = FALSE
    )

    # Save prose summaries as text
    writeLines(
      unlist(report$summaries),
      file.path(output_path, "rule_summaries.txt")
    )

    # Save full report as RDS
    saveRDS(report, file.path(output_path, "summary_report.rds"))

    message("\nResults saved to: ", output_path)
  }

  invisible(report)
}


# =============================================================================
# PART 6: ACCURACY EVALUATION WITH ORIGINAL DATA
# =============================================================================

#' Evaluate rule accuracy against original data
#'
#' This function requires the original training data to calculate true
#' false positive and false negative rates.
#'
#' @param report Output from summarize_crop_tree_rules()
#' @param original_data Data frame with columns: spp, site, drate, dbh, cr, best_log, crop
#' @return Data frame with detailed accuracy metrics
evaluate_rule_accuracy <- function(report, original_data) {


  if (is.null(original_data)) {
    stop("original_data is required for accuracy evaluation")
  }

  required_cols <- c("spp", "site", "drate", "dbh", "cr", "best_log", "crop")
  missing_cols <- setdiff(required_cols, names(original_data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in original_data: ", paste(missing_cols, collapse = ", "))
  }

  message("Evaluating accuracy against original data...")

  # Get the simplified rules
  simplified <- report$simplified_rules

  # For each species/site/drate combination, apply the simplified rule
  # and compare to actual outcomes
  accuracy_results <- list()

  for (i in 1:nrow(simplified)) {
    row <- simplified[i, ]

    # Filter original data to this combination
    data_subset <- original_data %>%
      filter(spp == row$spp, site == row$site, abs(drate - row$drate) < 0.001)

    if (nrow(data_subset) == 0) next

    # Apply simplified rule
    # Use the overall thresholds
    dbh_max <- row$dbh_max_overall
    cr_min <- row$cr_min_overall

    # Handle NA thresholds
    if (is.na(dbh_max)) dbh_max <- Inf
    if (is.na(cr_min)) cr_min <- -Inf

    # Predict crop status
    pred_crop <- (data_subset$dbh <= dbh_max) & (data_subset$cr >= cr_min)
    actual_crop <- as.logical(data_subset$crop)

    # Calculate confusion matrix
    tp <- sum(pred_crop & actual_crop, na.rm = TRUE)
    fp <- sum(pred_crop & !actual_crop, na.rm = TRUE)
    tn <- sum(!pred_crop & !actual_crop, na.rm = TRUE)
    fn <- sum(!pred_crop & actual_crop, na.rm = TRUE)

    n <- tp + fp + tn + fn

    accuracy_results[[i]] <- data.frame(
      spp = row$spp,
      site = row$site,
      drate = row$drate,
      n = n,
      true_positive = tp,
      false_positive = fp,
      true_negative = tn,
      false_negative = fn,
      accuracy = (tp + tn) / n,
      precision = tp / (tp + fp),
      recall = tp / (tp + fn),
      specificity = tn / (tn + fp),
      f1_score = 2 * tp / (2 * tp + fp + fn),
      false_positive_rate = fp / (fp + tn),
      false_negative_rate = fn / (fn + tp),
      stringsAsFactors = FALSE
    )
  }

  accuracy_df <- do.call(rbind, accuracy_results)

  # Print summary
  message("\n=== ACCURACY EVALUATION RESULTS ===\n")

  summary_by_spp <- accuracy_df %>%
    group_by(spp) %>%
    summarise(
      n_total = sum(n),
      mean_accuracy = mean(accuracy, na.rm = TRUE),
      mean_precision = mean(precision, na.rm = TRUE),
      mean_recall = mean(recall, na.rm = TRUE),
      mean_f1 = mean(f1_score, na.rm = TRUE),
      mean_fpr = mean(false_positive_rate, na.rm = TRUE),
      mean_fnr = mean(false_negative_rate, na.rm = TRUE),
      .groups = "drop"
    )

  print(summary_by_spp)

  message("\nInterpretation:")
  message("- FPR (False Positive Rate): Proportion of non-crop trees incorrectly marked as crop")
  message("- FNR (False Negative Rate): Proportion of actual crop trees missed by the rule")
  message("- Precision: Of trees marked as crop, what proportion truly are crop trees")
  message("- Recall: Of actual crop trees, what proportion did the rule identify")

  accuracy_df
}


# =============================================================================
# Example usage (commented out)
# =============================================================================

# # Run the summarization
# results <- summarize_crop_tree_rules("dataset2")
#
# # If you have the original data, evaluate accuracy:
# # (You would need to recreate crops_all from run.R)
# # accuracy <- evaluate_rule_accuracy(results, crops_all)
