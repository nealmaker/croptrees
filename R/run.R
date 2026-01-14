# use realistic stumpage prices for the base analysis
stupper <- T
pulppositive <- F

# range of discount rates to try
drates <- seq(.02, .06, by = .01)

source("R/data.R")
source("R/simulation.R")
source("R/lev.R")
source("R/fct-croptrees.R")

# start with a default discount rate and price factor ##########################
crops_all <- crop_trees(sim$trees, .03, 1, levs = lev_lookup)
rm(list = setdiff(ls(), c("crops_all", "crop_trees", "sim")))

# split into sw and hw and do the coarse analysis for each #####################
type <- "SW"
crops <- crops_all[crops_all$sw, ]
crops <- dplyr::select(crops, !"sw")
source("R/coarse-analysis.R")

rm(list = setdiff(ls(), c("crops_all", "crop_trees", "sim")))
type <- "HW"
crops <- crops_all[!crops_all$sw, ]
crops <- dplyr::select(crops, !"sw")
source("R/coarse-analysis.R")

rm(list = setdiff(ls(), c("crops_all", "crop_trees", "sim")))

# make a decision tree for each site/spp combo #################################
# later, can group species within a site if their species-specific decision
# trees are similar
source("R/fct-decision-tree.R")
results <- list()

for (site_type in unique(crops_all$site)) {
  for (species in unique(crops_all$spp[crops_all$site == site_type])) {
    for (rate in unique(crops_all$drate[crops_all$site == site_type & crops_all$spp == species])) {

      # Filter to this combination
      subset_data <- crops_all %>%
        dplyr::filter(spp == species, site == site_type, drate == rate) |>
        dplyr::select(spp, dbh, cr, best_log, site, drate, crop)

      # Skip if no data for this combination
      if (nrow(subset_data) == 0) next

      # Fit tree and save outputs
      tree_obj <- decision_tree(subset_data)

      # Store in list - use the name from the object
      if (inherits(tree_obj, "rpart")) {
        # It's an actual tree
        result_name <- attr(tree_obj, "name")
      } else {
        # It's an edge case list
        result_name <- tree_obj$name
      }
      results[[result_name]] <- tree_obj
    }
  }
}

# now vary discount rates and keep default price factor ########################
crops_all <- crop_trees(sim$trees, .02, 1, levs = lev_lookup)
for (dr in c(.04, .05, .06)) {
  crops_all <- rbind(crops_all, crop_trees(sim$trees, dr, 1, levs = lev_lookup))
}

results <- list()

for (site_type in unique(crops_all$site)) {
  for (species in unique(crops_all$spp[crops_all$site == site_type])) {
    for (rate in unique(crops_all$drate[crops_all$site == site_type & crops_all$spp == species])) {

      # Filter to this combination
      subset_data <- crops_all %>%
        dplyr::filter(spp == species, site == site_type, drate == rate) |>
        dplyr::select(spp, dbh, cr, best_log, site, drate, crop)

      # Skip if no data for this combination
      if (nrow(subset_data) == 0) next

      # Fit tree and save outputs
      tree_obj <- decision_tree(subset_data)

      # Store in list - use the name from the object
      if (inherits(tree_obj, "rpart")) {
        # It's an actual tree
        result_name <- attr(tree_obj, "name")
      } else {
        # It's an edge case list
        result_name <- tree_obj$name
      }
      results[[result_name]] <- tree_obj
    }
  }
}


################################################################################
# Based on what we've found, we can narrow the data down
# (using only interesting spp and sites) & Use finer DBH graduations to get more
# meaningful output. Also only use ST & veneer, since pulp trees are never crops

# need old lev_lookup
rm(list = setdiff(ls(), c("lev_lookup")))

# use realistic stumpage prices for the base analysis
stupper <- T
pulppositive <- F

# range of discount rates to try
drates <- seq(.02, .06, by = .01)

source("R/data2.R")
source("R/simulation.R")
source("R/fct-croptrees.R")
source("R/fct-decision-tree.R")

# set new directory for output
outdir <- "dataset2"

# vary discount rates and keep default price factor ########################
crops_all <- crop_trees(sim$trees, .02, 1, levs = lev_lookup)
for (dr in c(.03, .04, .05, .06)) {
  crops_all <- rbind(crops_all, crop_trees(sim$trees, dr, 1, levs = lev_lookup))
}

results <- list()

for (site_type in unique(crops_all$site)) {
  for (species in unique(crops_all$spp[crops_all$site == site_type])) {
    for (rate in unique(crops_all$drate[crops_all$site == site_type & crops_all$spp == species])) {

      # Filter to this combination
      subset_data <- crops_all %>%
        dplyr::filter(spp == species, site == site_type, drate == rate) |>
        dplyr::select(spp, dbh, cr, best_log, site, drate, crop)

      # Skip if no data for this combination
      if (nrow(subset_data) == 0) next

      # Fit tree and save outputs
      tree_obj <- decision_tree(subset_data, outdir = outdir)

      # Store in list - use the name from the object
      if (inherits(tree_obj, "rpart")) {
        # It's an actual tree
        result_name <- attr(tree_obj, "name")
      } else {
        # It's an edge case list
        result_name <- tree_obj$name
      }
      results[[result_name]] <- tree_obj
    }
  }
}


# Mortality Sensitivity: try 75% mortality #####################################
# set new directory for output
outdir <- "dat2_mort75pct"

# keep default drate and price factor ########################
crops_all <- crop_trees(sim$trees, .03, 1, levs = lev_lookup, mortality_factor = .75)

results <- list()

for (site_type in unique(crops_all$site)) {
  for (species in unique(crops_all$spp[crops_all$site == site_type])) {
    for (rate in unique(crops_all$drate[crops_all$site == site_type & crops_all$spp == species])) {

      # Filter to this combination
      subset_data <- crops_all %>%
        dplyr::filter(spp == species, site == site_type, drate == rate) |>
        dplyr::select(spp, dbh, cr, best_log, site, drate, crop)

      # Skip if no data for this combination
      if (nrow(subset_data) == 0) next

      # Fit tree and save outputs
      tree_obj <- decision_tree(subset_data, outdir = outdir)

      # Store in list - use the name from the object
      if (inherits(tree_obj, "rpart")) {
        # It's an actual tree
        result_name <- attr(tree_obj, "name")
      } else {
        # It's an edge case list
        result_name <- tree_obj$name
      }
      results[[result_name]] <- tree_obj
    }
  }
}

# Mortality Sensitivity: try 50% mortality #####################################
# set new directory for output
outdir <- "dat2_mort50pct"

# keep default drate and price factor ########################
crops_all <- crop_trees(sim$trees, .03, 1, levs = lev_lookup, mortality_factor = .5)

results <- list()

for (site_type in unique(crops_all$site)) {
  for (species in unique(crops_all$spp[crops_all$site == site_type])) {
    for (rate in unique(crops_all$drate[crops_all$site == site_type & crops_all$spp == species])) {

      # Filter to this combination
      subset_data <- crops_all %>%
        dplyr::filter(spp == species, site == site_type, drate == rate) |>
        dplyr::select(spp, dbh, cr, best_log, site, drate, crop)

      # Skip if no data for this combination
      if (nrow(subset_data) == 0) next

      # Fit tree and save outputs
      tree_obj <- decision_tree(subset_data, outdir = outdir)

      # Store in list - use the name from the object
      if (inherits(tree_obj, "rpart")) {
        # It's an actual tree
        result_name <- attr(tree_obj, "name")
      } else {
        # It's an edge case list
        result_name <- tree_obj$name
      }
      results[[result_name]] <- tree_obj
    }
  }
}

# Mortality Sensitivity: now try 0% mortality ##################################
# set new directory for output
outdir <- "dat2_mort0pct"

# keep default drate and price factor ########################
crops_all <- crop_trees(sim$trees, .03, 1, levs = lev_lookup, mortality_factor = .5)

results <- list()

for (site_type in unique(crops_all$site)) {
  for (species in unique(crops_all$spp[crops_all$site == site_type])) {
    for (rate in unique(crops_all$drate[crops_all$site == site_type & crops_all$spp == species])) {

      # Filter to this combination
      subset_data <- crops_all %>%
        dplyr::filter(spp == species, site == site_type, drate == rate) |>
        dplyr::select(spp, dbh, cr, best_log, site, drate, crop)

      # Skip if no data for this combination
      if (nrow(subset_data) == 0) next

      # Fit tree and save outputs
      tree_obj <- decision_tree(subset_data, outdir = outdir)

      # Store in list - use the name from the object
      if (inherits(tree_obj, "rpart")) {
        # It's an actual tree
        result_name <- attr(tree_obj, "name")
      } else {
        # It's an edge case list
        result_name <- tree_obj$name
      }
      results[[result_name]] <- tree_obj
    }
  }
}
