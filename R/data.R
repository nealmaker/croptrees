requireNamespace("forestmaker")
requireNamespace("magrittr")
requireNamespace("ranger")
requireNamespace("Rborist")
requireNamespace("dplyr")

# define tree variables ########################################################
# species of interest chosen from forestmaker species
species <- levels(forestmaker::simtrees_sample$spp)[
  c(5, 10, 11, 12, 18, 19, 22, 23, 27, 28)]

dbhs <- seq(4, 22, by = 3)

crs <- seq(20, 70, by = 10)

# potential log grades from forestmaker are '1' veneer, '2' saw log (#2 or
# better), '3' tie/mat log, or '5' fuelwood/pulp

# list combinations for bottom 3 (8') log segments, which drive
# tree values
grades <- c("112", "122", "155", "515",
            "222", "225", "255", "525",
            "333", "355", "555")
# concatenate potential grades, assuming all segments above bottom three are
# fuelwood/pulp, to get 10-digit grade calls that are used in forestmaker
grades <- paste0(grades, "5555555")


# define sites #################################################################
sites <- list(
  # rich northern hardwood site on Salmon-Adamant complex soils along the
  # Winooski River in Moretown VT, deposited in Glacial Lake Winooski.
  richhw = list(lat = 44.316, lon = -72.7423, site_class = 4, elev = 700),
  # beech-red maple hardwood site on convex mountain slope in Bartlett NH,
  # Monadnock very stony granitic till soils. look up how that forest is
  # classified
  tillmixed = list(lat = 44.094, lon = -71.268, site_class = 6, elev = 1500),
  # sandy well-drained piney site at Paul Smith's College Forest
  sandyoutwash = list(lat = 2, lon = -72, site_class = 4, elev = 1300),
  # lowland spruce-fir forest in Maine somewhere
  lowspfr = list(lat = 2, lon = -72, site_class = 4, elev = 1300)
)



# compile dataset ##############################################################
dat <- expand.grid(spp = species, dbh = dbhs, cr = crs, grade = grades) |>
  # assume the tree in question is alone in the plot. 24.07 is scalar to convert
  # ba/plot to ba/acre
  dplyr::mutate(tpa = 24.07, ba_ac = (0.005454 * dbh ^ 2) * tpa, cumsurv = 1,
                ba = ba_ac, bal = 0)

data_by_site <- lapply(sites, function(x) {
  # add site attributes
  y <- dat |> dplyr::mutate(site_class = x$site_class, lat = x$lat, lon = x$lon,
                            elev = x$elev)
  # estimate tree heights using a random forest model built into forestmaker
  y$height <- forestmaker::est_ht(y)
  return(y)
})


# set parameters to guide simulations ##########################################
params <- forestmaker::params_default

# mill prices based on Jan 2025 Log Street Journal reports for NY, VT, NH, ME,
# Canada
params$prices$mill_grade2[params$prices$spp == "black cherry"] <- 325
params$prices$mill_grade2[params$prices$spp == "fir"] <- 370
params$prices$mill_grade2[params$prices$spp == "hard maple"] <- 575
params$prices$mill_grade2[params$prices$spp == "hemlock"] <- 270
params$prices$mill_grade2[params$prices$spp == "paper birch"] <- 375
params$prices$mill_grade2[params$prices$spp == "red oak"] <- 525
params$prices$mill_grade2[params$prices$spp == "soft maple"] <- 400
params$prices$mill_grade2[params$prices$spp == "spruce"] <- 370
params$prices$mill_grade2[params$prices$spp == "white pine"] <- 300
params$prices$mill_grade2[params$prices$spp == "yellow birch"] <- 400

# keep default pulp/firewood prices of 160/mbf hw & 125/mbf sw

# stumpage equation uses trucking cost of $75/mbf (observed by PBF) and assumes
# that stumpage/mbf equals .6 * (roadside price - $60)
params$truckcost <- 75
params$stump <- forestmaker::stump_factory(.6, 60, method = "linear")

# dbh at which to stop growing any given tree
max_dbh <- 30
# max simulation length in years
params$endyr <- 200


# write simulator function #####################################################
# will eventually return data frame of trees at each timestep and their
# undiscounted values, now just returns df without values
get_values <- function(dat, params, max_dbh) {
  sim <- dat <- dat |> dplyr::mutate(year = 0)
  year <- 0
  repeat {
    year <- year + params$steplength
    index <- which(dat$dbh < max_dbh)
    dat[index, ] <- dat_new <-
      forestmaker::grow(dat[index, ], params, models = "base")
    dat$ba <- dat$ba_ac
    dat_new$year <- year
    sim <- rbind(sim, dat_new)
    if (all(dat$dbh >= max_dbh)) break
    if (year > params$endyr) break
  }
  return(sim)
}

# TO DO: Get values into get_values; add function that ruins current grade if
# crown (based on CR) extends into bolt; parallelize with some testing
