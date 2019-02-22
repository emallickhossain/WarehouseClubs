# Constructs how often toilet paper is bought by itself and how often it is not bought on a trip
library(data.table)
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "7260Purch.csv"))
fullTrips <- NULL
for (i in 2004:2016) {
  trips <- fread(paste0(path, "Trips/trips", i, ".csv"),
                 select = c("household_code", "panel_year", "trip_code_uc", "total_spent"))
  tpTrips <- tpPurch[panel_year == i, .(trip_code_uc, total_price_paid)]
  trips <- merge(trips, tpTrips, by = "trip_code_uc", all.x = TRUE)
  trips[is.na(total_price_paid), "total_price_paid" := 0]
  trips[, "tpInTrip" := ifelse(total_price_paid == 0, 0L, 1L)]
  trips[, "tpOnly" := ifelse(total_price_paid == total_spent, 1L, 0L)]
  fullTrips <- rbind(fullTrips, trips)
}

# Tallying the trips
tally <- fullTrips[, .(tpTrips = sum(tpInTrip),
                       tpOnly = sum(tpOnly),
                       totalTrips = .N), by = .(household_code, panel_year)]
tally[, "nonTPTrips" := (1 - tpTrips / totalTrips) * 100]
tally[, "toOnlyShare" := (tpOnly / totalTrips) * 100]
