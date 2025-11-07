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
                                        TRUE ~ "clayplain")) |>
  dplyr::select(tree, spp, dbh, cr, logs, site, value, year)
