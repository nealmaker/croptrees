requireNamespace("forestmaker")
requireNamespace("magrittr")
requireNamespace("ranger")
requireNamespace("Rborist")

# define tree variables ########################################################
species <- levels(simtrees_sample$spp)[
  c(1, 2, 5, 10, 11, 12, 18, 19, 22, 23, 25, 26, 27, 28)]

dbhs <- seq(4, 30, by = 2)

crs <- seq(10, 100, by = 10)

logses <- "1125555555" # how to vary this in a meaningful way?

# compile dataset ##############################################################

