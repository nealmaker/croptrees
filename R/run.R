# use realistic stumpage prices for the base analysis
stupper <- T
pulppositive <- F

source("R/simulation.R")
source("R/fct-croptrees.R")

# start with a default discount rate and price factor ##########################
crops_all <- crop_trees(sim$trees, .03, 1)
rm(list = setdiff(ls(), c("crops_all", "crop_trees")))

# split into sw and hw and do the coarse analysis for each #####################
type <- "SW"
crops <- crops_all[crops_all$sw, ]
crops <- dplyr::select(crops, !"sw")
source("R/coarse-analysis.R")

rm(list = setdiff(ls(), c("crops_all", "crop_trees")))
type <- "HW"
crops <- crops_all[!crops_all$sw, ]
crops <- dplyr::select(crops, !"sw")
source("R/coarse-analysis.R")
