source("R/simulation.R")
source("R/fct-croptrees.R")
library(tidymodels)
library(tidyverse)
library(ranger)
library(vip)
library(pdp)
library(patchwork)  # for combining plots

# start with a default discount rate and price factor ##########################
crops <- crop_trees(sim, .03, 1)

# random forest model of what's a crop tree, at defaults #######################
# Analysis of crop tree determination across species, sites, and quality factors
################################################################################

# Set seed for reproducibility
set.seed(123)

# ============================================================================
# 1. DATA PREPARATION
# ============================================================================

# Convert to factors - tidymodels works well with factors for categorical variables
data <- crops %>%
  mutate(
    # Categorical variables as factors
    spp = factor(spp),
    site = factor(site),
    logs = factor(logs),
    best_log = factor(best_log),
    worst_log = factor(worst_log),
    butt_log = factor(butt_log),
    veneer_present = factor(veneer_present),
    saw_present = factor(saw_present),
    crop = factor(crop, levels = c("FALSE", "TRUE"))  # outcome variable
  ) %>%
  # Remove drate and price_factor since they don't vary in this phase
  select(-drate, -price_factor)

# Quick data summary
cat("\n=== Data Summary ===\n")
print(glimpse(data))
cat("\nCrop tree distribution:\n")
print(table(data$crop))
cat("\nProportion crop trees:", mean(data$crop == "TRUE"), "\n")

# ============================================================================
# 2. BUILD RANDOM FOREST MODEL
# ============================================================================

cat("\n=== Building Random Forest Model ===\n")

# Define model specification
rf_spec <- rand_forest(
  trees = 500,
  mtry = NULL,  # will use default (sqrt of predictors for classification)
  min_n = 5     # minimum observations in terminal nodes
) %>%
  set_engine("ranger",
             importance = "impurity",  # for variable importance
             num.threads = parallel::detectCores() - 1,  # use multiple cores
             probability = TRUE) %>%  # get probability predictions for PDPs
  set_mode("classification")

# Fit the model
rf_fit <- rf_spec %>%
  fit(crop ~ ., data = data)

cat("Model fitted successfully!\n")
cat("Out-of-bag error rate:", rf_fit$fit$prediction.error, "\n")

# ============================================================================
# 3. VARIABLE IMPORTANCE ANALYSIS
# ============================================================================

cat("\n=== Variable Importance Analysis ===\n")

# Create variable importance plot
vip_plot <- vip(rf_fit, num_features = 20, aesthetics = list(fill = "steelblue")) +
  labs(title = "Variable Importance for Crop Tree Classification",
       subtitle = "Based on mean decrease in Gini impurity") +
  theme_minimal(base_size = 12)

print(vip_plot)
ggsave("output/base-rate-rf-analysis/01_variable_importance.png", vip_plot, width = 10, height = 8)

# Get importance scores as data frame for further analysis
importance_df <- vip::vi(rf_fit) %>%
  arrange(desc(Importance))

print(importance_df)

# ============================================================================
# 4. PARTIAL DEPENDENCE PLOTS
# ============================================================================

cat("\n=== Creating Partial Dependence Plots ===\n")

# For continuous variables: dbh, cr, quality_score
# Create proper prediction wrapper for pdp with tidymodels

# Extract the underlying ranger model and create wrapper
rf_engine <- extract_fit_engine(rf_fit)

pred_wrapper <- function(object, newdata) {
  # Predict probabilities using the ranger engine directly
  pred <- predict(object, data = newdata)
  # For probability=TRUE, ranger returns a matrix of probabilities
  # We want the probability of "TRUE" class (second column)
  pred$predictions[, "TRUE"]
}

# DBH partial dependence
cat("Computing partial dependence for DBH...\n")
pdp_dbh <- partial(rf_engine,
                   pred.var = "dbh",
                   train = data,
                   pred.fun = pred_wrapper,
                   grid.resolution = 20)

p_dbh <- pdp_dbh |> ggplot() +
  geom_line(aes(dbh, yhat, group = yhat.id), alpha = .01) +
  geom_smooth(aes(dbh, yhat), color = "steelblue", linewidth = 1.2) +
  labs(title = "Partial Dependence: DBH",
       y = "Probability of Crop Tree",
       x = "DBH (inches)") +
  theme_minimal()

# Crown ratio partial dependence
cat("Computing partial dependence for Crown Ratio...\n")
pdp_cr <- partial(rf_engine,
                  pred.var = "cr",
                  train = data,
                  pred.fun = pred_wrapper,
                  grid.resolution = 20)

p_cr <- pdp_cr |> ggplot() +
  geom_line(aes(cr, yhat, group = yhat.id), alpha = .01) +
  geom_smooth(aes(cr, yhat), color = "forestgreen", linewidth = 1.2) +
  labs(title = "Partial Dependence: Crown Ratio",
       y = "Probability of Crop Tree",
       x = "Crown Ratio (%)") +
  theme_minimal()

# Quality score partial dependence
cat("Computing partial dependence for Quality Score...\n")
pdp_quality <- partial(rf_engine,
                       pred.var = "quality_score",
                       train = data,
                       pred.fun = pred_wrapper,
                       grid.resolution = 20)

p_quality <- pdp_quality |> ggplot() +
  geom_line(aes(quality_score, yhat, group = yhat.id), alpha = .01) +
  geom_smooth(aes(quality_score, yhat), color = "darkgoldenrod", linewidth = 1.2) +
  labs(title = "Partial Dependence: Quality Score",
       y = "Probability of Crop Tree",
       x = "Quality Score") +
  theme_minimal()

# Combine PDP plots
pdp_combined <- p_dbh + p_cr + p_quality +
  plot_annotation(title = "Partial Dependence Plots for Key Continuous Variables")

print(pdp_combined)
ggsave("output/base-rate-rf-analysis/02_partial_dependence_continuous.png", pdp_combined,
       width = 15, height = 5)

# Two-way interaction: DBH x Crown Ratio
cat("\nCreating 2-way interaction plot (this may take a moment)...\n")
pdp_interaction <- partial(rf_engine,
                           pred.var = c("dbh", "cr"),
                           train = data,
                           pred.fun = pred_wrapper,
                           grid.resolution = 15)

pdp_int_plot <- ggplot(pdp_interaction, aes(x = dbh, y = cr, fill = yhat)) +
  geom_tile() +
  geom_contour(aes(z = yhat), color = "white", alpha = 0.8) +
  scale_fill_viridis_c(name = "P(Crop Tree)", option = "viridis", labels = scales::percent) +
  labs(title = "Interaction: DBH × Crown Ratio",
       x = "DBH (inches)",
       y = "Crown Ratio (%)") +
  theme_minimal()

print(pdp_int_plot)
ggsave("output/base-rate-rf-analysis/03_interaction_dbh_cr.png", pdp_int_plot, width = 8, height = 6)

# Two-way interaction: DBH x Quality Score
cat("\nCreating second 2-way interaction plot (this may take a moment)...\n")
pdp_interaction2 <- partial(rf_engine,
                            pred.var = c("dbh", "quality_score"),
                            train = data,
                            pred.fun = pred_wrapper,
                            grid.resolution = 15)

pdp_int_plot2 <- ggplot(pdp_interaction2, aes(x = dbh, y = quality_score, fill = yhat)) +
  geom_tile() +
  geom_contour(aes(z = yhat), color = "white", alpha = 0.8) +
  scale_fill_viridis_c(name = "P(Crop Tree)", option = "viridis", labels = scales::percent) +
  labs(title = "Interaction: DBH × Quality",
       x = "DBH (inches)",
       y = "Quality Score") +
  theme_minimal()

print(pdp_int_plot2)
ggsave("output/base-rate-rf-analysis/04_interaction_dbh_quality.png", pdp_int_plot2, width = 8, height = 6)

# ============================================================================
# 5. CATEGORICAL VARIABLE ANALYSIS
# ============================================================================

cat("\n=== Analyzing Categorical Variables ===\n")

# Species effect
species_summary <- data %>%
  group_by(spp) %>%
  summarize(
    n = n(),
    crop_rate = mean(crop == "TRUE"),
    .groups = "drop"
  ) %>%
  arrange(desc(crop_rate))

p_species <- ggplot(species_summary, aes(x = reorder(spp, crop_rate), y = crop_rate)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = scales::percent(crop_rate, accuracy = 0.1)),
            hjust = -0.2, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Crop Tree Rate by Species",
       x = "Species",
       y = "Proportion Crop Trees") +
  theme_minimal()

# Site effect
site_summary <- data %>%
  group_by(site) %>%
  summarize(
    n = n(),
    crop_rate = mean(crop == "TRUE"),
    .groups = "drop"
  ) %>%
  arrange(desc(crop_rate))

p_site <- ggplot(site_summary, aes(x = reorder(site, crop_rate), y = crop_rate)) +
  geom_col(fill = "forestgreen", alpha = 0.8) +
  geom_text(aes(label = scales::percent(crop_rate, accuracy = 0.1)),
            hjust = -0.2, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Crop Tree Rate by Site",
       x = "Site Type",
       y = "Proportion Crop Trees") +
  theme_minimal()

# Quality variables
quality_summary <- data %>%
  group_by(butt_log) %>%
  summarize(
    n = n(),
    crop_rate = mean(crop == "TRUE"),
    .groups = "drop"
  )

p_veneer <- ggplot(quality_summary, aes(x = butt_log, y = crop_rate)) +
  geom_col(fill = "darkgoldenrod", alpha = 0.8) +
  geom_text(aes(label = scales::percent(crop_rate, accuracy = 0.1)),
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Crop Tree Rate: Butt Log Grade",
       x = "Grade of Butt Log",
       y = "Proportion Crop Trees") +
  theme_minimal()

# Combine categorical plots
cat_combined <- (p_species + p_site) / p_veneer +
  plot_annotation(title = "Crop Tree Rates by Categorical Variables")

print(cat_combined)
ggsave("output/base-rate-rf-analysis/05_categorical_analysis.png", cat_combined, width = 12, height = 10)

# ============================================================================
# 6. MULTIDIMENSIONAL VISUALIZATION
# ============================================================================

cat("\n=== Creating Multidimensional Visualizations ===\n")

# DBH x Crown Ratio faceted by species
p_facet_species <- ggplot(data, aes(x = dbh, y = cr, fill = crop)) +
  geom_tile(alpha = 0.1) +
  facet_wrap(~spp, ncol = 4) +
  scale_fill_manual(values = c("black", "steelblue")) +
  labs(title = "Crop Tree Status by DBH and Crown Ratio",
       subtitle = "Faceted by Species",
       x = "DBH (inches)",
       y = "Crown Ratio (%)",
       color = "Status") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

print(p_facet_species)
ggsave("output/base-rate-rf-analysis/06_dbh_cr_by_species.png", p_facet_species, width = 14, height = 10)

# DBH x Crown Ratio faceted by site
p_facet_site <- ggplot(data, aes(x = dbh, y = cr, fill = crop)) +
  geom_tile(alpha = 0.1) +
  facet_wrap(~site, ncol = 2) +
  scale_fill_manual(values = c("black", "steelblue")) +
  labs(title = "Crop Tree Status by DBH and Crown Ratio",
       subtitle = "Faceted by Site Type",
       x = "DBH (inches)",
       y = "Crown Ratio (%)",
       color = "Status") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p_facet_site)
ggsave("output/base-rate-rf-analysis/07_dbh_cr_by_site.png", p_facet_site, width = 10, height = 8)

# Quality score effect across species
p_quality_species <- ggplot(data, aes(x = quality_score, fill = crop)) +
  geom_histogram(position = "fill", bins = 15, alpha = 0.8) +
  facet_wrap(~spp, ncol = 4) +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "steelblue"),
                    labels = c("Not Crop", "Crop Tree")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Crop Tree Proportion by Quality Score and Species",
       x = "Quality Score",
       y = "Proportion",
       fill = "Status") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

print(p_quality_species)
ggsave("output/base-rate-rf-analysis/08_quality_by_species.png", p_quality_species, width = 14, height = 10)

# ============================================================================
# 7. LOG QUALITY DECOMPOSITION ANALYSIS
# ============================================================================

cat("\n=== Analyzing Log Quality Components ===\n")

# Butt log effect
butt_log_summary <- data %>%
  group_by(butt_log) %>%
  summarize(
    n = n(),
    crop_rate = mean(crop == "TRUE"),
    .groups = "drop"
  ) %>%
  arrange(butt_log)

p_butt <- ggplot(butt_log_summary, aes(x = factor(butt_log), y = crop_rate)) +
  geom_col(fill = "darkgoldenrod", alpha = 0.8) +
  geom_text(aes(label = scales::percent(crop_rate, accuracy = 0.1)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Crop Tree Rate by Butt Log Grade",
       x = "Butt Log Grade",
       y = "Proportion Crop Trees") +
  theme_minimal()

# Best log effect
best_log_summary <- data %>%
  group_by(best_log) %>%
  summarize(
    n = n(),
    crop_rate = mean(crop == "TRUE"),
    .groups = "drop"
  ) %>%
  arrange(best_log)

p_best <- ggplot(best_log_summary, aes(x = factor(best_log), y = crop_rate)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = scales::percent(crop_rate, accuracy = 0.1)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Crop Tree Rate by Best Log Grade",
       x = "Best Log Grade",
       y = "Proportion Crop Trees") +
  theme_minimal()

# veneer effect
veneer_summary <- data %>%
  group_by(veneer_present) %>%
  summarize(
    n = n(),
    crop_rate = mean(crop == "TRUE"),
    .groups = "drop"
  )

p_veneer <- ggplot(veneer_summary, aes(x = factor(veneer_present), y = crop_rate)) +
  geom_col(fill = "forestgreen", alpha = 0.8) +
  geom_text(aes(label = scales::percent(crop_rate, accuracy = 0.1)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Crop Tree Rate by Veneer Presence",
       x = "Is Veneer Present?",
       y = "Proportion Crop Trees") +
  theme_minimal()

# sawlog effect
saw_summary <- data %>%
  group_by(saw_present) %>%
  summarize(
    n = n(),
    crop_rate = mean(crop == "TRUE"),
    .groups = "drop"
  )

p_saw <- ggplot(saw_summary, aes(x = factor(saw_present), y = crop_rate)) +
  geom_col(fill = "#790b02", alpha = 0.8) +
  geom_text(aes(label = scales::percent(crop_rate, accuracy = 0.1)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Crop Tree Rate by Sawlog Presence",
       x = "Are Sawlogs Present?",
       y = "Proportion Crop Trees") +
  theme_minimal()

log_quality_combined <- p_butt + p_best + p_veneer + p_saw +
  plot_annotation(title = "Crop Tree Rates by Log Quality Components")

print(log_quality_combined)
ggsave("output/base-rate-rf-analysis/09_log_quality_components.png", log_quality_combined,
       width = 12, height = 12)

# Actual Log Calls
logs_summary <- data %>%
  group_by(logs) %>%
  summarize(
    n = n(),
    crop_rate = mean(crop == "TRUE"),
    .groups = "drop"
  ) %>%
  arrange(desc(crop_rate))

p_logs <- ggplot(logs_summary, aes(x = reorder(logs, crop_rate), y = crop_rate)) +
  geom_col(fill = "gray25", alpha = 0.8) +
  geom_text(aes(label = scales::percent(crop_rate, accuracy = 0.1)),
            hjust = -0.2, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Crop Tree Rate by Grade Calls",
       x = "Grades",
       y = "Proportion Crop Trees") +
  theme_minimal()

print(p_logs)
ggsave("output/base-rate-rf-analysis/10_log_grade_analysis.png", p_logs, width = 6, height = 8)
# ============================================================================
# 8. MODEL PREDICTIONS AND CONFUSION ANALYSIS
# ============================================================================

cat("\n=== Analyzing Model Predictions ===\n")

# Add predictions to data
data_with_pred <- data %>%
  bind_cols(
    predict(rf_fit, data),
    predict(rf_fit, data, type = "prob")
  )

# Confusion matrix
conf_mat <- conf_mat(data_with_pred, truth = crop, estimate = .pred_class)
print(conf_mat)

# Calculate metrics
metrics <- metric_set(accuracy, sens, precision, recall, f_meas)
model_metrics <- metrics(data_with_pred, truth = crop, estimate = .pred_class)
print(model_metrics)

# Plot confusion matrix
autoplot(conf_mat, type = "heatmap") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix: Crop Tree Classification") +
  theme_minimal()
ggsave("output/base-rate-rf-analysis/11_confusion_matrix.png", width = 8, height = 6)

# Analyze misclassifications
misclassified <- data_with_pred %>%
  filter(crop != .pred_class) %>%
  mutate(error_type = case_when(
    crop == "TRUE" & .pred_class == "FALSE" ~ "False Negative",
    crop == "FALSE" & .pred_class == "TRUE" ~ "False Positive"
  ))

cat("\nMisclassification summary:\n")
cat("Total misclassifications:", nrow(misclassified), "\n")
cat("False negatives (missed crop trees):", sum(misclassified$error_type == "False Negative"), "\n")
cat("False positives (incorrectly predicted crop):", sum(misclassified$error_type == "False Positive"), "\n")

# Visualize misclassifications
if (nrow(misclassified) > 0) {
  p_misclass <- ggplot(misclassified, aes(x = dbh, y = cr, color = error_type)) +
    geom_jitter(alpha = 0.6, size = 2.4) +
    facet_wrap(~spp) +
    scale_color_manual(values = c("False Negative" = "steelblue", "False Positive" = "red")) +
    labs(title = "Misclassified Trees by DBH and Crown Ratio",
         subtitle = "Faceted by Species",
         x = "DBH (inches)",
         y = "Crown Ratio (%)",
         color = "Error Type") +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p_misclass)
  ggsave("output/base-rate-rf-analysis/12_misclassifications.png", p_misclass, width = 14, height = 10)
}

# ============================================================================
# 9. SUMMARY STATISTICS AND EXPORT
# ============================================================================

cat("\n=== Summary Statistics ===\n")

# Overall summary by key factors
summary_table <- data %>%
  group_by(spp, site) %>%
  summarize(
    n_obs = n(),
    crop_rate = mean(crop == "TRUE"),
    mean_dbh = mean(dbh),
    mean_cr = mean(cr),
    mean_quality = mean(quality_score),
    .groups = "drop"
  ) %>%
  arrange(spp, site)

print(summary_table)
write_csv(summary_table, "output/base-rate-rf-analysis/species_site_summary.csv")

# Save model for later use
saveRDS(rf_fit, "output/base-rate-rf-analysis/rf_model.rds")

# save data for later use
saveRDS(data, "output/base-rate-rf-analysis/croptree_data.rds")

cat("\n=== Analysis Complete ===\n")
cat("All plots saved to output/base-rate-rf-analysis directory\n")
cat("Model saved as: output/base-rate-rf-analysis/rf_model.rds\n")
cat("Summary table saved as: output/base-rate-rf-analysis/species_site_summary.csv\n")
