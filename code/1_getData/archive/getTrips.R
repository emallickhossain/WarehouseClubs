# Gets trips data. Dropping trips not in the panel year and only keeping trips for
# households in cleaned panel data
library(data.table)
library(purrr)
yr <- 2004:2016
path <- "/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_2004-2016/nielsen_extracts/HMS/"
panel <- fread("/home/mallick/Desktop/Nielsen/Data/Clean/fullPanel.csv",
               select = c("household_code", "panel_year"),
               key = c("household_code", "panel_year"))

getTrips <- function(yr) {
  trips <- fread(paste0(path, yr, "/Annual_Files/trips_", yr, ".tsv"),
                 key = c("household_code", "panel_year"))
  trips <- trips[panel_year == substr(purchase_date, 1, 4)]
  trips <- merge(trips, panel, by = c("household_code", "panel_year"))
  fwrite(trips, paste0("/home/mallick/Desktop/Nielsen/Data/Clean/Trips/trips", yr, ".csv"))
}
map(yr, getTrips)
