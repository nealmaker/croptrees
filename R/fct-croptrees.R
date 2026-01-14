# function to determine which trees in the simulation data are crop trees, based
# on given discount rate and price factors

# rate is a decimal (3 pct discount rate = .03, ...)

# price_factor is a multiplier (2 is rates are twice the 2025 base rate used to
# do sim, 0.5 is half, ...)

# levs is lookup table of LEVs

# mortality_factor multiplies the rate of mortality for sensitivity analysis
# 0.5 cuts mortality in half, 2 doubles mortality, etc.

# lev_factor multiplies the LEV values for sensitivity analysis
# 0.5 halves LEV (e.g., less productive replacement stand), 2 doubles it, etc.
crop_trees <- function(data, rate, price_factor, levs,
                       mortality_factor = 1, lev_factor = 1) {
  data$drate <- rate
  data <- dplyr::left_join(data, levs, by = c("site", "drate"))
  data$value <- data$value_nomort -
    (mortality_factor * (data$value_nomort - data$value))
  data$value <- data$value * price_factor
  # Apply LEV factor
  data$lev <- data$lev * lev_factor
  # PV is present value if cut @ t = data$year: includes LEV captured when cut
  data$pv <- (data$value / (1 + rate) ^ data$year) +
    (data$lev / (1 + rate) ^ data$year)

  out <- data |> dplyr::group_by(tree) |>
    dplyr::summarize(sw = sw[1],
                     spp = spp[1],
                     dbh = dbh[year == 0],
                     cr = cr[year == 0],
                     logs = logs[year == 0],
                     best_log = best_log[year == 0],
                     worst_log = worst_log[year == 0],
                     butt_log = butt_log[year == 0],
                     veneer_present = veneer_present[year == 0],
                     saw_present = saw_present[year == 0],
                     quality_score = quality_score[year == 0],
                     site = site[1],
                     # has to be worth growing at least 15 more years to be crop
                     crop = max(pv) > pv[year == 0] &
                       max(pv) > pv[year == 5] &
                       max(pv) > pv[year == 10]) |>
    dplyr::mutate(drate = rate,
                  price_factor = price_factor,
                  spp = mods$canonical_feature_spec$categories$spp[spp]) |>
    dplyr::select(spp, sw, dbh, cr, logs, best_log, worst_log,
                  butt_log, veneer_present, saw_present, quality_score,
                  site, drate, price_factor, crop)
}
