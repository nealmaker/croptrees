# use realistic stumpage prices for the base analysis
stupper <- T
pulppositive <- F

# range of discount rates to try
drates <- seq(.02, .06, by = .01)

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

      # Store in list with informative name
      result_name <- attr(tree_obj, "name")
      results[[result_name]] <- tree_obj
    }
  }
}

