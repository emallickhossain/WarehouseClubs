# Imputes the ZIP of stores based on the modal ZIP of shoppers
library(data.table)
library(purrr)
library(furrr)
plan(multiprocess)

path <- "/scratch/upenn/hossaine/"
yr <- 2004:2016
panel <- fread(paste0(path, "fullPanel.csv"), select = c("household_code", "panel_year", "zip_code"))
zipLatLon <- fread(paste0(path, "zipLatLon.csv"))

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

getData <- function(yr) {
  # Getting Nielsen purchases by trip
  purch <- unique(fread(paste0(path, "Purchases/purchase", yr, ".csv"), select = c("trip_code_uc")))

  # Merging trips
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year",
                            "retailer_code", "store_code_uc", "store_zip3"))
  trips <- trips[store_code_uc != 0]
  purch <- merge(trips, purch, by = "trip_code_uc")
  purch[, "trip_code_uc" := NULL]
  return(purch)
}

fullData <- rbindlist(future_map(yr, getData))
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))

zipImpute <- fullData[, .(zipImpute = Mode(zip_code)),
                      by = .(retailer_code, store_code_uc)]
zipImpute <- merge(zipImpute, zipLatLon, by.x = "zipImpute", by.y = "zip_code")
setnames(zipImpute, c("lat", "lon"), c("store_lat", "store_lon"))
fwrite(zipImpute, paste0(path, "zipImpute.csv"))
