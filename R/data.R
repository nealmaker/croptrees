requireNamespace("forestgrower")
requireNamespace("treemodeler")
requireNamespace("timbertally")
requireNamespace("magrittr")
requireNamespace("dplyr")
requireNamespace("tidyr")

# load models ##################################################################
options("treemodeler.registry_path" = "C:/Users/neal/projects/forestmaker-models")
mods <- treemodeler::initialize_models("5.3", c("base", "base", "slim"),
                                       c("growth", "height", "taper"),
                                       cr_mode = "derived")

# define tree variables ########################################################
# species of interest chosen from forestmaker species: black cherry, fir, hard
# maple, hemlock, paper birch, red maple, red oak, red spruce, white oak, white
# pine, yellow birch
species <- unique(forestgrower::params_default$prices$spp)[
  c(11, 21, 23, 25, 52, 54, 55, 58, 68, 73, 75)]

# define the softwoods
sw <- species[c(2, 4, 8, 10)]

dbhs <- seq(4, 22, by = 3)

crs <- seq(20, 70, by = 10)

# potential log grades from forestmaker are '1' veneer, '2' saw log (#2 or
# better), '3' tie/mat log, or '5' fuelwood/pulp

# list combinations for bottom 3 (8') log segments, which drive
# tree values
grades <- c("112", "122", "215", "155", "515",
            "222", "225", "255", "525",
            "333", "355", "555")
# concatenate potential grades, assuming all segments above bottom three are
# fuelwood/pulp, to get 10-digit grade calls that are used in forestmaker
grades <- paste0(grades, "5555555")


# define sites #################################################################
sites <- list(
  # hardwood-white pine site on Becket sandy loam soils at Paul Smith's College
  # Forest.
  ganitictill = list(lat = 44.463, lon = -74.211, site_class = 5, elev = 1300),

  # rich northern hardwood site on deep, loamy Salmon-Adamant complex soils
  # along the Winooski River in Moretown VT, deposited in Glacial Lake Winooski.
  richnhw = list(lat = 44.316, lon = -72.7423, site_class = 4, elev = 700),

  # beech-red maple hardwood site on convex mountain slope in Bartlett NH,
  # Monadnock very stony granitic till soils.
  # LEFT OUT BECAUSE IT'S VERY SIMILAR TO GRANITIC TILL
  # mountaintill = list(lat = 44.094, lon = -71.268, site_class = 5, elev = 1500),

  # lowland spruce-fir site on poorly-drained Brayton lodgment till soil in
  # Dallas Maine.
  lowlandspfr = list(lat = 45.009, lon = -70.606, site_class = 6, elev = 1700),

  # mesic clayplain site on Vergennes clay at the Williams Woods Natural Area,
  # Charlotte, VT
  clayplain = list(lat = 44.275, lon = -73.243, site_class = 5, elev = 120)
)



# compile dataset ##############################################################
dat <- expand.grid(spp = species, dbh = dbhs, cr = crs, logs = grades) |>
  # remove softwood trees with veneer
  dplyr::filter(!(spp %in% sw & grepl("1", logs))) |>
  # assume the tree in question is alone in the plot. 24.07 is scalar to convert
  # ba/plot to ba/acre
  dplyr::mutate(spp = as.character(spp), logs = as.character(logs),
                tpa = 24.07, ba_ac = (0.005454 * dbh ^ 2) * tpa, cumsurv = 1,
                ba = ba_ac, bal = 0)
# make upper bolts sawlogs in softwoods
substr(dat$logs[dat$spp %in% sw], 4, 10) <- "2222222"

dat <- lapply(sites, function(x) {
  # add site attributes
  y <- dat |> dplyr::mutate(site_class = x$site_class, lat = x$lat, lon = x$lon,
                            elev = x$elev)
  # estimate tree heights using a random forest model built into forestmaker
  y$height <- treemodeler::predict_ht(y, mods)
  return(y)
})
dat <- do.call(rbind, dat)
dat$tree <- dat$plot <- dat$stand <- 1:nrow(dat)
dat$live <- TRUE
dat$sw <- dat$spp %in% sw


# set parameters to guide simulations ##########################################
params <- forestgrower::params_default
# filter only applicable species in params and remove duplicates
params$prices <- params$prices[params$prices$spp %in% species, ]
for (i in unique(params$prices$spp)) {
  idx <- params$prices$spp == i
  p <- params$prices[idx, ][1, ]
  params$prices <- rbind(params$prices[!idx, ], p)
}
rm(p, idx)

# mill prices based on Jan 2025 Log Street Journal reports for NY, VT, NH, ME,
# Canada

# params$prices$mill_grade2[params$prices$spp == "black cherry"] <- 325
# params$prices$mill_grade2[params$prices$spp == "fir"] <- 370
# params$prices$mill_grade2[params$prices$spp == "hard maple"] <- 575
# params$prices$mill_grade2[params$prices$spp == "hemlock"] <- 270
# params$prices$mill_grade2[params$prices$spp == "paper birch"] <- 375
# params$prices$mill_grade2[params$prices$spp == "red oak"] <- 525
# params$prices$mill_grade2[params$prices$spp == "red maple"] <- 400
# params$prices$mill_grade2[params$prices$spp == "red spruce"] <- 370
# params$prices$mill_grade2[params$prices$spp == "white oak"] <- 480
# params$prices$mill_grade2[params$prices$spp == "white pine"] <- 300
# params$prices$mill_grade2[params$prices$spp == "yellow birch"] <- 400

# Instead of using current species-specific prices, decided to use current
# averages for broad species groups (medium value hardwoods, high value
# hardwoods, pine/hemlock, and softwoods), since price relationships between
# those groups have been shown to roughly hold over time, but prices of specific
# species within each group vary relative to one-another.
params$prices$mill_grade2[params$prices$spp_grp == "mvh"] <- 406
params$prices$mill_grade2[params$prices$spp_grp == "hvh"] <- 456
params$prices$mill_grade2[params$prices$spp_grp == "pine_hem"] <- 400 # 285 is right; this is a test
params$prices$mill_grade2[params$prices$spp_grp == "sw"] <- 400 # 370 is right; this is a test

# pulp/firewood prices of 125/mbf for hardwoods & 45/mbf for softwoods, roadside
# (~$62.50/cd & $18/ton, respectively) based on 2024 Pekin Branch Forestry
# records.
params$prices$pulp_roadside <- 125
params$prices$pulp_roadside[treemodeler::softwood(params$prices$spp, mods)] <- 10 # 45 is right; this is a test

# stumpage equation uses trucking cost of $75/mbf (from 2024 Pekin Branch
# Forestry records) and estimates that stumpage ($/mbf) = .6 * (roadside price -
# $60). This is a reasonable estimate for a higher value operation, in the
# experience of Maker and Foppert.
params$truckcost <- 75
params$stump <- timbertally::stump_factory(.6, 60, method = "linear")

# dbh at which to stop growing any given tree
max_dbh <- 30
# max simulation length in years
params$endyr <- 200


# encode species to match models ###############################################
dat <-
  treemodeler::encode_simulation_inputs(dat, mods$region,
                                        mods$canonical_feature_spec)
params$prices <-
  treemodeler::encode_simulation_inputs(params$prices, mods$region,
                                        mods$canonical_feature_spec)

# add values to data now that params are set ###################################
dat$value <- timbertally::stumpage(dat, params$stump, params,
                                   params$truckcost, mods)


# write simulator function #####################################################

# returns data frame of trees at each timestep and their undiscounted values
get_values <- function(dat, params, max_dbh, mods) {
  sim <- dat <- dat |> dplyr::mutate(year = 0)
  year <- 0
  repeat {
    year <- year + params$steplength
    index <- which(dat$dbh < max_dbh)
    dat[index, ] <- dat_new <-
      forestgrower::grow(dat[index, ], params, mods)
    # CONSIDER CR FUNCTION THAT ASSUMES CROWN BASE IS FIXED AT TIME 0
    # would be valid if crop trees remain free to grow throughout life
    # maybe more accurate in this case than random forest model?
    dat$ba <- dat$ba_ac
    dat_new$ba <- dat_new$ba_ac
    dat_new$year <- year


    # ruin grades if log is in crown, only for hardwoods
    dat_new[!dat_new$sw, ] <-
      dat_new[!dat_new$sw, ] |>
      dplyr::mutate(
        # ruin bolt grades in dat_new if bolts are branchy, based on ht and cr
        # IF THIS IS KEPT, IT SHOULD ONLY APPLY TO HARDWOODS!!!!!!!!!!!!!!!!
        crown_base = height * (100 - cr) / 100,
        logs = dplyr::case_when(
          crown_base < 9.5 ~ "5555555555",
          crown_base < 18 ~ paste0(substr(logs, 1, 1), "555555555"),
          crown_base < 26.5 ~ paste0(substr(logs, 1, 2), "55555555"),
          TRUE ~ logs)
      ) |>
      dplyr::select(!crown_base)

    # get undiscounted stumpage values, modified by cumulative probability of
    # survival
    dat_new$value = timbertally::stumpage(dat_new, params$stump, params, params$truckcost, mods) *
      dat_new$cumsurv

    # add data for new timestep to data for previous timesteps
    sim <- rbind(sim, dat_new)

    if (all(dat$dbh >= max_dbh)) break
    if (year > params$endyr) break
  }
  return(sim)
}
