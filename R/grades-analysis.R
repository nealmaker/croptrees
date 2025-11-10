source("R/simulation.R")
library(ggplot2)

logs <- mutate(sim$logs, spp = mods$canonical_feature_spec$categories$spp[spp],
               tree = as.factor(tree))
sim <- mutate(sim$trees, spp = mods$canonical_feature_spec$categories$spp[spp],
              tree = as.factor(tree))

sp <- unique(sim$spp)
for (i in sp) {
  sw <- ifelse(filter(sim, spp == i)$sw[1], "SW", "HW")
  # get a var set such that only grades vary and there are only
  # two or (for hardwoods) three log grades calls
  if (sw == "SW") {
    # logcalls <- c("5552222222", "2222222222")
    logcalls <- c("5555555555", "2225555555")
  } else {
    logcalls <- c("5555555555", "3335555555", "2225555555", "1125555555")
  }
  t_ids <- filter(sim, year == 0, spp == i, dbh == 10, cr == 40,
                  logs %in% logcalls, site == "granitictill")$tree
  sim_i <- filter(sim, tree %in% t_ids) |>
    # use starting grades, since big crowns can degrade upper logs later on
    group_by(tree) |>
    mutate(logs_s = logs[1],
           maxyr = year[which(value == max(value))]) |>
    ungroup()

  # plot values over time
  p <- sim_i |> ggplot(aes(year, value, col = logs_s)) + geom_line(size = .8) +
    geom_vline(aes(xintercept = maxyr, col = logs_s), size = 2, alpha = .5) +
    # geom_line(aes(year, value_nomort, col = logs_s), linetype = "dashed") +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_minimal(base_size = 12) +
    labs(title = paste(i, "Value Growth by Timber Quality"),
         subtitle = "10 in trees w/ 40% CR on a granitic till site",
         y = "Value (USD) mortality adjusted")

  print(p)
  # ggsave(paste0("output/base-rate-rf-analysis/", sw,
  #               "/values-by-spp-and-grade/", i, "-.png"),
  #        p, width = 7, height = 7)
}
