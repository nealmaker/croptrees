source("R/data.R")

# test by varying just the crown ratio #########################################
test <- dat |>
  dplyr::filter(spp == dat$spp[2], dbh == 10, logs == "1225555555",
                lat == dat$lat[1])

out <- get_values(test, params, max_dbh, mods)

requireNamespace("ggplot2")
out |> ggplot2::ggplot(ggplot2::aes(year, value, colour = as.factor(tree))) +
  ggplot2::geom_line()
out |> ggplot2::ggplot(ggplot2::aes(year, cumsurv, colour = as.factor(tree))) +
  ggplot2::geom_line()

rm(test, out)


# simulations for the whole enchilada ##########################################
out <- get_values(dat, params, max_dbh, mods)
