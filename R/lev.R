# For each site:
# 1) Filter Simulation Data to two or three possible replacement trees
#   (trees expected to replace the crop tree and get grown to maturity in future
#    rotations, under careful management)
#
# 2) For each discount rate:
#   a) For each replacement tree:
#     - calculate its NPV absent LEV
#     - use the resulting NPV as LEV and recalculate its NPV, inclusive of LEV
#     - repeat until LEV converges
#   b) Examine differences in LEV between replacement trees, for sens. analysis
#   c) Use weighted average of LEVs to get standard LEV for site/drate combo
#
# 3) Build data frame with, site, discount rate, and LEV
#
# Combine site tables into a single LEV lookup table

# Replacement tree definitions based on expert opinion, with reference to
# Thompson, Sorenson & Zaino. 2019, Wetland, Woodland, Wildland ... &
# Jenkins. 2025. Field Guide to the Woody Plants of the Northern Forest
# (see upcoming Ecological Patterns of the Northern Forest Interpretive Guide)

# if not sourced from run.R define settings to use realistic stumpage values
if (!("stupper" %in% ls())) stupper <- T
if (!("pulppositive" %in% ls())) pulppositive <- F
if (!("drates" %in% ls())) drates <- seq(.02, .06, by = .01)
# source("R/simulation.R")

sites <- unique(sim$trees$site)
sitetabs <- lapply(sites, function(i) {
  if (i == "granitictill") {
    t1 <- filter(sim$trees, site == i, year == 0, dbh == 7, cr == 40,
                 spp == which(mods$canonical_feature_spec$categories$spp == "WP"), # white pine
                 logs == "2222222222")$tree
    t2 <- filter(sim$trees, site == i, year == 0, dbh == 4, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "RM"), # red maple
                 logs == "2225555555")$tree
    t3 <- filter(sim$trees, site == i, year == 0, dbh == 4, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "YB"), # yellow birch
                 logs == "1225555555")$tree
    treeids <- c(t1, t2, t3)
    trees <- filter(sim$trees, tree %in% treeids)
    weights <- c(.45, .25, .3)
  } else if (i == "richnhw") {
    t1 <- filter(sim$trees, site == i, year == 0, dbh == 4, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "BM"), # sugar maple
                 logs == "1225555555")$tree
    t2 <- filter(sim$trees, site == i, year == 0, dbh == 7, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "YB"), # yellow birch
                 logs == "1225555555")$tree
    t3 <- filter(sim$trees, site == i, year == 0, dbh == 4, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "RM"), # red maple
                 logs == "2225555555")$tree
    treeids <- c(t1, t2, t3)
    trees <- filter(sim$trees, tree %in% treeids)
    weights <- c(.6, .35, .05)
  } else if (i == "mountaintill") {
    t1 <- filter(sim$trees, site == i, year == 0, dbh == 4, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "BM"), # sugar maple
                 logs == "1225555555")$tree
    t2 <- filter(sim$trees, site == i, year == 0, dbh == 7, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "YB"), # yellow birch
                 logs == "1225555555")$tree
    t3 <- filter(sim$trees, site == i, year == 0, dbh == 4, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "RM"), # red maple
                 logs == "2225555555")$tree
    treeids <- c(t1, t2, t3)
    trees <- filter(sim$trees, tree %in% treeids)
    weights <- c(.35, .35, .3)
  } else if (i == "lowlandspfr") {
    t1 <- filter(sim$trees, site == i, year == 0, dbh == 4, cr == 50,
                 spp == which(mods$canonical_feature_spec$categories$spp == "RS"), # red spruce
                 logs == "2222222222")$tree
    t2 <- filter(sim$trees, site == i, year == 0, dbh == 4, cr == 50,
                 spp == which(mods$canonical_feature_spec$categories$spp == "BF"), # balsam fir
                 logs == "2222222222")$tree
    t3 <- filter(sim$trees, site == i, year == 0, dbh == 4, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "RM"), # red maple
                 logs == "2225555555")$tree
    treeids <- c(t1, t2, t3)
    trees <- filter(sim$trees, tree %in% treeids)
    weights <- c(.475, .475, .05)
  } else if (i == "clayplain") {
    t1 <- filter(sim$trees, site == i, year == 0, dbh == 7, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "SO"), # northern red oak
                 logs == "1225555555")$tree
    t2 <- filter(sim$trees, site == i, year == 0, dbh == 7, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "WO"), # white oak (Q. alba)
                 logs == "1225555555")$tree
    t3 <- filter(sim$trees, site == i, year == 0, dbh == 4, cr == 30,
                 spp == which(mods$canonical_feature_spec$categories$spp == "BM"), # sugar maple
                 logs == "1225555555")$tree
    treeids <- c(t1, t2, t3)
    trees <- filter(sim$trees, tree %in% treeids)
    weights <- c(.65, .3, .05)
  }

  drate_tabs <- lapply(drates, function(j) {
    # Trees start at about 20 yrs old, but want NPV age 0
    trees$npv <- trees$value / ((1 + j) ^ (trees$year + 20))
    spp_tabs <- lapply(treeids, function(k) {
      onetree <- trees[trees$tree == k, ]
      npv <- max(onetree$npv)
      rotation <- onetree$year[which(onetree$npv == npv)]
      lev <- npv / (1 - ((1 + j) ^ -(rotation + 20)))

      repeat {
        if (rotation < 2 * params$steplength) break
        rotation <- rotation - params$steplength
        npv <- onetree$npv[which(onetree$year == rotation)]
        lev2 <- npv / (1 - ((1 + j) ^ -(rotation + 20)))

        if (lev > lev2) break

        lev <- lev2
      }

      # return df w/ 1 row & 4 cols: site, drate, spp, lev
      return(data.frame(site = i, drate = j,
                        spp = mods$canonical_feature_spec$categories$spp[onetree$spp[[1]]],
                        lev = lev))
    })

    spp_tab <- do.call(rbind, spp_tabs)
    spp_tab$weight <- weights

    # save .csv of spp_tab for sensitivity by site rate, & spp
    ratechar <- substr(as.character(j), nchar(as.character(j)), nchar(as.character(j)))
    write.csv(spp_tab,
              file = paste0("output/replacement-tree-lev/", i, "-",
                            ratechar, "PctDRate.csv"))

    # return df w/ 1 row per tree & 4 cols: site, drate, spp, lev
    return(data.frame(site = i, drate = j,
                      lev = sum(spp_tab$lev * spp_tab$weight)))
  })

  drate_tab <- do.call(rbind, drate_tabs)
  return(drate_tab)
})

lev_lookup <- do.call(rbind, sitetabs)

# load all csv files and combine for analysis: find all combos of site & rate,
# then load into list objects based on naming conventions
# then rbind all list objects
x <- expand.grid(site = sites, drate = substr(as.character(drates), nchar(as.character(drates)), nchar(as.character(drates))))
spptabs <- list(NA)
for (i in 1:nrow(x)) {
  tab <- read.csv(paste0("output/replacement-tree-lev/", x$site[i], "-",
                         x$drate[i], "PctDRate.csv"))
  spptabs[[i]] <- tab
}

spp_tab <- do.call(rbind, spptabs) |> select(site, drate, spp, lev, weight)
write.csv(spp_tab,
          file = "output/replacement-tree-lev/00-tree-LEVs-combined.csv",
          row.names = F)
