source("R/data.R")

# test by varying just the crown ratio #########################################
test <- dat |>
  dplyr::filter(spp == dat$spp[3], dbh == 10, logs == "1225555555",
                lat == dat$lat[1])

out <- get_values(test, params, max_dbh, mods)

requireNamespace("ggplot2")
out |> ggplot2::ggplot(ggplot2::aes(year, value, colour = as.factor(tree))) +
  ggplot2::geom_line()
out |> ggplot2::ggplot(ggplot2::aes(year, cumsurv, colour = as.factor(tree))) +
  ggplot2::geom_line()

rm(test, out)


# simulations for the whole enchilada ##########################################
sim <- get_values(dat, params, max_dbh, mods)


# simplify output to play with data ############################################
sim <- sim |>
  dplyr::mutate(site = dplyr::case_when(lat == 44.463 ~ "granitictill",
                                        lat == 44.316 ~ "richnhw",
                                        lat == 45.009 ~ "lowlandspfr",
                                        TRUE ~ "clayplain"),
                best_log = min(as.numeric())) |>
  dplyr::select(tree, spp, dbh, cr, logs, site, value, year)

# add some simplified grade variables
# highest grade in bottom 3 bolts
sim$best_log <- sapply(strtrim(sim$logs, 3), function(i) {
  min(as.numeric(unlist(strsplit(i, ""))))
})
# lowest grade in bottom 3 bolts
sim$worst_log <- sapply(strtrim(sim$logs, 3), function(i) {
  max(as.numeric(unlist(strsplit(i, ""))))
})
# grade of butt log
sim$butt_log <- as.numeric(strtrim(sim$logs, 1))
# is veneer present (boolean)
sim$veneer_present <- grepl("1", strtrim(sim$logs, 3))
# is sawtimber present (boolean)
sim$saw_present <- sim$veneer_present | grepl("2", strtrim(sim$logs, 3))
# weighted sum score (low grade scores 0, tie 1, sawtimber 2, veneer 3)
sim$quality_score <- sapply(strtrim(sim$logs, 3), function(i) {
  grades <- as.numeric(unlist(strsplit(i, "")))
  scores <- rep(0, 3)
  scores <- ifelse(grades == 3, 1, scores)
  scores <- ifelse(grades == 2, 2, scores)
  scores <- ifelse(grades == 1, 3, scores)
  sum(scores)
})

sim <- dplyr::select(sim, tree, spp, dbh, cr, logs, best_log, worst_log,
                     butt_log, veneer_present, saw_present, quality_score,
                     site, value, year)
