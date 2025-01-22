requireNamespace("forestmaker")
requireNamespace("magrittr")
requireNamespace("ranger")
requireNamespace("Rborist")
requireNamespace("dplyr")

# define tree variables ########################################################
# species of interest chosen from forestmaker species
species <- levels(forestmaker::simtrees_sample$spp)[
  c(2, 5, 10, 11, 12, 18, 19, 22, 23, 25, 26, 27, 28)]

dbhs <- seq(4, 30, by = 2)

crs <- seq(10, 100, by = 10)

# potential log grades from forestmaker are '1' veneer, '2' saw log (#2 or
# better), '3' tie/mat log, or '5' fuelwood/pulp
grades <- c(1, 2, 3, 5)
# list all possible combinations for bottom 3 (8') log segments, which drive
# tree values
grades <- expand.grid(grades, grades, grades)
# concatenate potential grades, assuming all segments above bottom three are
# fuelwood/pulp, to get 10-digit grade calls that are used in forestmaker
grades <- paste0(grades[, 1], grades[, 2], grades[, 3], "5555555")


# compile dataset ##############################################################
# temporarily define arbitrary site conditions to test CPU times with
dat <- expand.grid(spp = species, dbh = dbhs, cr = crs, grade = grades) |>
  # assume the tree in question is alone in the plot. 24.07 is scalar to convert
  # ba/plot to ba/acre
  dplyr::mutate(tpa = 24.07, ba_ac = (0.005454 * dbh ^ 2) * tpa, cumsurv = 1,
                ba = ba_ac, bal = 0, site_class = 6, lat = 44.2, lon = -72.8,
                elev = 1000)

# estimate tree heights using a random forest model built into forestmaker
dat$height <- forestmaker::est_ht(dat)


# set parameters to guide simulations ##########################################
params <- forestmaker::params_default
# dbh at which to stop growing any given tree
max_dbh <- 30


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
  }
  return(sim)
}


# test simulation times to see if this dataset is too much to work with ########
# x <- get_values(dat[1,], params, max_dbh = 10)
# View(x)

# using max dbh makes for a lot of computation on slow growing trees that are
# clearly not crop trees. A 4" tree with 10% cr grows more than 600 years before
# reaching 10", with negligible survival rate.

# Here we try some 10" trees with 40% cr, growing to 30" dbh
time10 <- system.time(get_values(dat[11506:11515,], params, max_dbh = 30))
# elapsed time 171.39 sec (2.9 min) for 10 trees, without any value calculations
# in get_values yet (Jan 20 first try)

# note: replicated this within development version of forestmaker with similar
# results. Running in separate R project is not affecting run time.

# NEED DIFFERENT ENDING CRITERIA AND SMALLER DATASET
# This setup would take more than a year to run.
