# For troubleshooting, make upper bolts pulp in softwoods and
# keep stumpage rates positive for all logs >= 5"
stupper <- F
pulppositive <- T

source("R/simulation.R")
library(ggplot2)
library(patchwork)  # for combining plots

logs <- mutate(sim$logs, spp = mods$canonical_feature_spec$categories$spp[spp],
               tree = as.factor(tree))
trees <- mutate(sim$trees, spp = mods$canonical_feature_spec$categories$spp[spp],
              tree = as.factor(tree))

# values over time, varying grades #############################################
sp <- unique(trees$spp)
for (i in sp) {
  sw <- ifelse(filter(trees, spp == i)$sw[1], "SW", "HW")
  # get a var set such that only grades vary and there are only
  # two or (for hardwoods) three log grades calls
  # if (sw == "SW") {
  #   logcalls <- c("5552222222", "2222222222")
  #   # logcalls <- c("5555555555", "2225555555")
  # } else {
  #   logcalls <- c("5555555555", "3335555555", "2225555555", "1125555555")
  # }
  logcalls <- c("5555555555", "5552222222", "3335555555", "2225555555",
                "2222222222", "1125555555")
  t_ids <- filter(trees, year == 0, spp == i, dbh == 10, cr == 40,
                  logs %in% logcalls, site == "granitictill")$tree
  trees_i <- filter(trees, tree %in% t_ids) |>
    # use starting grades, since big crowns can degrade upper logs later on
    group_by(tree) |>
    mutate(logs_s = logs[1],
           maxyr = year[which(value == max(value))]) |>
    ungroup()

  # plot values over time
  p <- trees_i |> ggplot(aes(year, value, col = logs_s)) + geom_line(size = .8) +
    geom_vline(aes(xintercept = maxyr, col = logs_s), size = 2, alpha = .5) +
    # geom_line(aes(year, value_nomort, col = logs_s), linetype = "dashed") +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_minimal(base_size = 12) +
    labs(title = paste(i, "Value Growth by Timber Quality"),
         subtitle = '10" trees w/ 40% CR on a granitic till site',
         y = "Value (USD) mortality adjusted")

  print(p)
  ggsave(paste0("output/base-rate-rf-analysis/", sw,
                "/values-by-spp-and-grade/", i, ".png"),
         p, width = 7, height = 7)
}

# Bolt by bolt comparisons for pulp and sawtimber ##############################
for (i in sp) {
  sw <- ifelse(filter(trees, spp == i)$sw[1], "SW", "HW")

  logcalls <- c("5555555555", "5552222222", "2225555555", "2222222222")
  t_temp <- filter(trees, year == 0, spp == i, dbh == 10, cr == 40,
                   logs %in% logcalls, site == "granitictill")
  t_ids <- t_temp$tree
  grades <- ifelse(grepl("2", t_temp$logs), "saw in butt", "all pulp")

  trees_i <- filter(trees, tree %in% t_ids) |>
    mutate(cumsurv = value / value_nomort)

  logs_i <- logs[logs$tree %in% t_ids,] |>
    mutate(grade = grades[match(tree, t_ids)])
  logs_i <- left_join(logs_i, select(trees_i, tree, year, cumsurv), by = c("tree", "year"))

  # bolt volumes through time
  p_bolt_vol <- logs_i |> ggplot(aes(year, vol_log, col = as.factor(section))) +
    geom_line() + facet_wrap(~ grade) +
    scale_color_brewer("bolt", type = "qual", palette = "Dark2") +
    labs(title = "Volume Growth by Bolt",
         y = "bolt volume (cd)")

  # bolt unit value through time
  p_bolt_rate <- logs_i |> ggplot(aes(year, stumprt, col = as.factor(section))) +
    geom_line() + facet_wrap(~ grade) +
    scale_color_brewer("bolt", type = "qual", palette = "Dark2") +
    labs(title = "Stumpage Rate Changes by Bolt",
         y = "stumpage rate ($/cd)")

  # Combine multi-bolt plots
  p_multi_bolt <- p_bolt_vol + p_bolt_rate +
    plot_annotation(title = paste(i, "Log Bolt Attributes"),
                    subtitle = '10" trees w/ 40% CR on a granitic till site')

  print(p_multi_bolt)
  ggsave(paste0("output/base-rate-rf-analysis/", sw, "/bolt-analysis/", i, ".png"),
         p_multi_bolt, width = 15, height = 5)

  sections <- 1:4
  p_boltval <- as.list(sections)
  for (j in sections) {
    p_boltval[[j]] <- list(nomort = NA, mort = NA)
    # bolt value through time w/o mortality
    p_boltval[[j]][[1]] <- logs_i |> filter(section == j) |> ggplot(aes(year, stumprt * vol_log)) +
      geom_line(col = "steelblue", size = .8) +
      # geom_line(aes(year, stumprt * vol_log * cumsurv), col = "darkorange") +
      facet_grid(grade ~ .) +
      scale_color_brewer("bolt", type = "qual", palette = "Dark2") +
      labs(title = paste("bolt", j, "w/o mortality"),
           y = "bolt value ($)")

    # bolt value through time w/ mortality
    p_boltval[[j]][[2]] <- logs_i |> filter(section == j) |>
      mutate(val_mort = stumprt * vol_log * cumsurv) |>
      group_by(grade) |>
      mutate(maxyr = year[val_mort == max(val_mort)]) |>
      ungroup() |>
      ggplot(aes(year, stumprt * vol_log)) +
      # geom_line(col = "steelblue", size = .8) +
      geom_line(aes(year, stumprt * vol_log * cumsurv), size = .8, col = "steelblue") +
      geom_vline(aes(xintercept = maxyr), col = "steelblue", size = 2, alpha = .6) +
      facet_grid(grade ~ .) +
      scale_color_brewer("bolt", type = "qual", palette = "Dark2") +
      labs(title = paste("bolt", j, "w/ mortality"),
           subtitle = 'vertical line at maximum value',
           y = "bolt value ($)")
  }

  p_boltval <- p_boltval[[1]][[1]] + p_boltval[[1]][[2]] +
    p_boltval[[2]][[1]] + p_boltval[[2]][[2]] +
    p_boltval[[3]][[1]] + p_boltval[[3]][[2]] +
    p_boltval[[4]][[1]] + p_boltval[[4]][[2]] +
    plot_layout(ncol = 2) +
    plot_annotation(title = paste(i, "Individual Bolt Values, With & Without Mortality"),
                    subtitle = '10" trees w/ 40% CR on a granitic till site, w/ & w/o sawtimber potential in the bottom 3 bolts',
                    caption = "Values are undiscounted. With mortality they are expected values \n(multiplied by the probability of tree survival).")

  print(p_boltval)
  ggsave(paste0("output/base-rate-rf-analysis/", sw, "/bolt-analysis/", i, "-bybolt.png"),
         p_boltval, width = 8, height = 12)
}

