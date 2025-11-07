source("R/simulation.R")
source("R/fct-croptrees.R")

# start with a default discount rate and price factor ##########################
crops <- crop_trees(sim, .03, 1)

# xgboost model of what's a crop tree, at defaults #############################
