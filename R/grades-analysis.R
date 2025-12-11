# For troubleshooting, make upper bolts pulp in softwoods and
# keep stumpage rates positive for all logs >= 5"
stupper <- F
pulppositive <- T

source("R/simulation.R")
library(ggplot2)
library(dplyr)
library(tidyr)
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

  logcalls <- c("5555555555", "5552222222", "2555555555", "2222222222")
  t_temp <- filter(trees, year == 0, spp == i, dbh == 10, cr == 40,
                   logs %in% logcalls, site == "granitictill")
  t_ids <- t_temp$tree
  t_id_st <- t_temp$tree[t_temp$logs %in% c("2555555555", "2222222222")]
  t_id_pulp <- t_temp$tree[t_temp$logs %in% c("5555555555", "5552222222")]
  grades <- ifelse(grepl("2", t_temp$logs), "saw in butt", "all pulp")

  trees_i <- filter(trees, tree %in% t_ids) |>
    mutate(cumsurv = value / value_nomort)

  logs_i <- logs[logs$tree %in% t_ids,] |>
    mutate(grade = grades[match(tree, t_ids)])
  logs_i <- left_join(logs_i, select(trees_i, tree, year, cumsurv), by = c("tree", "year"))

  # Whole tree plots -----------------------------------------------------------
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

  # Individual bolts plots -----------------------------------------------------
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
                    subtitle = '10" trees w/ 40% CR on a granitic till site, w/ & w/o sawtimber potential in the bottom bolt',
                    caption = "Values are undiscounted. With mortality they are expected values \n(multiplied by the probability of tree survival).")

  print(p_boltval)
  ggsave(paste0("output/base-rate-rf-analysis/", sw, "/bolt-analysis/", i, "-bybolt.png"),
         p_boltval, width = 8, height = 12)

  # Combined Bolts & total plots -----------------------------------------------
  logs_i_st <- filter(logs_i, tree == t_id_st)
  trees_i_st <- filter(trees_i, tree == t_id_st) |>
    select(year, value) |> rename(total_value = value)
  logs_i_st <- logs_i_st |> mutate(value = stumprt * vol_log * cumsurv) |>
    filter(section %in% 1:4) |>
    pivot_wider(id_cols = year, names_from = section, names_prefix = "bolt_", values_from = value) |>
    left_join(trees_i_st)
  logs_i_st$bolt_1[is.na(logs_i_st$bolt_1)] <- 0
  logs_i_st$bolt_2[is.na(logs_i_st$bolt_2)] <- 0
  logs_i_st$bolt_3[is.na(logs_i_st$bolt_3)] <- 0
  logs_i_st$bolt_4[is.na(logs_i_st$bolt_4)] <- 0
  logs_i_st$total_value[is.na(logs_i_st$total_value)] <- 0

  logs_i_p <- filter(logs_i, tree == t_id_pulp)
  trees_i_p <- filter(trees_i, tree == t_id_pulp) |>
    select(year, value) |> rename(total_value = value)
  logs_i_p <- logs_i_p |> mutate(value = stumprt * vol_log * cumsurv) |>
    filter(section %in% 1:4) |>
    pivot_wider(id_cols = year, names_from = section, names_prefix = "bolt_", values_from = value) |>
    left_join(trees_i_p)
  logs_i_p$bolt_1[is.na(logs_i_p$bolt_1)] <- 0
  logs_i_p$bolt_2[is.na(logs_i_p$bolt_2)] <- 0
  logs_i_p$bolt_3[is.na(logs_i_p$bolt_3)] <- 0
  logs_i_p$bolt_4[is.na(logs_i_p$bolt_4)] <- 0
  logs_i_p$total_value[is.na(logs_i_p$total_value)] <- 0

  plot_bolts_total <- function(logs, p_title) {
    logs |> ggplot(aes(year, bolt_1)) + geom_line(size = .8, col = "steelblue") +
      geom_line(aes(year, bolt_2), size = .8, col = "steelblue", alpha = .8) +
      geom_line(aes(year, bolt_3), size = .8, col = "steelblue", alpha = .6) +
      geom_line(aes(year, bolt_4), size = .8, col = "steelblue", alpha = .4) +
      geom_line(aes(year, total_value), size = 1.2, col = "red") +
      geom_vline(aes(xintercept = logs$year[which(logs$total_value == max(logs$total_value))]), size = 3, alpha = .5, col = "red") +
      geom_vline(aes(xintercept = logs$year[which(logs$bolt_1 == max(logs$bolt_1))]), col = "steelblue", size = .8, linetype = "dashed") +
      geom_vline(aes(xintercept = logs$year[which(logs$bolt_2 == max(logs$bolt_2))]), col = "steelblue", size = .8, linetype = "dashed", alpha = .8) +
      geom_vline(aes(xintercept = logs$year[which(logs$bolt_3 == max(logs$bolt_3))]), col = "steelblue", size = .8, linetype = "dashed", alpha = .6) +
      geom_vline(aes(xintercept = logs$year[which(logs$bolt_4 == max(logs$bolt_4))]), col = "steelblue", size = .8, linetype = "dashed", alpha = .4) +
      labs(title = p_title,
           y = "Value ($)")
  }

  pst <- plot_bolts_total(logs_i_st, "sawtimber in first bolt")
  ppulp <- plot_bolts_total(logs_i_p, "all pulp")

  # Combine plots
  p_bolts_and_totals <- pst + ppulp +
    plot_layout(ncol = 1) +
    plot_annotation(title = paste(i, "Bolt and Total Tree Values Through Time"),
                    subtitle = '10" trees w/ 40% CR on a granitic till site',
                    caption = "Blue lines are values of individual bolts with dashed vertical lines at maximums. \nDarker lines are lower bolts. \nRed line is combined value with vertical red line at the tree's maximum value. \nAll values are undiscounted and account for the likelihood of mortality.")

  print(p_bolts_and_totals)
  ggsave(paste0("output/base-rate-rf-analysis/", sw, "/bolt-analysis/", i, "with-totals.png"),
         p_bolts_and_totals, width = 7, height = 10)
}

