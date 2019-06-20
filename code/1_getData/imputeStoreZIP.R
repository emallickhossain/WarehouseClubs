# Imputes the ZIP of stores based on the modal ZIP of shoppers
library(data.table)
library(purrr)
threads <- 8

path <- "/scratch/upenn/hossaine/"
yr <- 2004:2017
panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "zip_code"))
zipLatLon <- fread(paste0(path, "zipLatLon.csv"))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Merging trips
trips <- fread(paste0(path, "fullTrips.csv"), nThread = threads,
               select = c("trip_code_uc", "household_code",
                          "panel_year", "store_code_uc"))[store_code_uc != 0]

getData <- function(yr) {
  # Getting Nielsen purchases by trip
  purch <- unique(fread(paste0(path, "fullPurch", yr, ".csv"),
                        nThread = threads, select = "trip_code_uc"))
  return(purch)
}
fullData <- rbindlist(map(yr, getData))
fullData <- merge(fullData, trips, by = "trip_code_uc")
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))

zipImpute <- fullData[, .(zipImpute = Mode(zip_code)), by = .(store_code_uc)]
zipImpute <- merge(zipImpute, zipLatLon, by.x = "zipImpute", by.y = "zip_code")
setnames(zipImpute, c("lat", "lon"), c("store_lat", "store_lon"))
fwrite(zipImpute, paste0(path, "zipImpute.csv"))
