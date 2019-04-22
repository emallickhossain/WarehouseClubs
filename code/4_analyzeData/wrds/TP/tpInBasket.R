# Constructs how often toilet paper is bought by itself and how often it is
# not bought on a trip. Also, I compute it's average basket share
library(data.table)
path <- "/scratch/upenn/hossaine/"

tpPurch <- fread(paste0(path, "7260Purch.csv"))[drop == 0]
fullTrips <- fread(paste0(path, "trips.csv"),
                   select = c("household_code", "panel_year", "trip_code_uc", "total_spent"))
tpTrips <- tpPurch[, .(trip_code_uc, total_price_paid)]
fullTrips <- merge(fullTrips, tpTrips, by = "trip_code_uc", all.x = TRUE)
fullTrips[is.na(total_price_paid), "total_price_paid" := 0]
fullTrips[, "tpInTrip" := ifelse(total_price_paid == 0, 0L, 1L)]
fullTrips[, "tpOnly" := ifelse(total_price_paid == total_spent, 1L, 0L)]

# Tallying the trips
tally <- fullTrips[, .(tpTrips = sum(tpInTrip),
                       tpOnly = sum(tpOnly),
                       totalTrips = .N,
                       tpPrice = mean(total_price_paid),
                       totalSpent = mean(total_spent)),
                   by = .(household_code, panel_year)]
tally[, "nonTPTrips" := (1 - tpTrips / totalTrips) * 100]
tally[, "tpOnlyShare" := (tpOnly / totalTrips) * 100]
tally[, "basketShare" := tpPrice / totalSpent * 100]
mean(tally$nonTPTrips)
mean(tally$tpOnlyShare)
mean(tally$basketShare)
