# Combines files into annual files for a good defined as a UPC
# I do not drop store codes = 0!
library(data.table)
library(purrr)
yr <- 2004:2016
panel <- fread("/home/mallick/Desktop/Nielsen/Data/Clean/fullPanel.csv",
               select = c("household_code", "panel_year", "projection_factor",
                          "household_income", "market", "age"))

combineDat <- function(yr) {
  print(yr)
  # Merging everything
  trips <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Clean/Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year", "purchase_date"))
  purch <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Clean/Purchases/purchase", yr, ".csv"))
  fullData <- merge(trips, purch, by = c("trip_code_uc"))[, "trip_code_uc" := NULL]
  fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))

  # Removing thin products (few transactions in each quarter)
  fullData[, "Ntrans" := .N, by = .(purchase_date, market, upc, upc_ver_uc)]
  fullData <- fullData[Ntrans >= 25]

  # Removing implausibly high price dispersion
  fullData[, "Pbar" := sum(p * quantity) / sum(quantity),
           by = .(upc, upc_ver_uc, market, purchase_date)]
  fullData[, "normP" := p / Pbar]
  fullData[, "CV" := sd(normP) / mean(normP),
           by = .(upc, upc_ver_uc, market, purchase_date)]
  fullData <- fullData[CV < 1]

  # Adding basket variables
  fullData[, "X" := sum(p * quantity),
           by = .(household_code, panel_year, purchase_date)]
  fullData[, "Xbar" := sum(Pbar * quantity),
           by = .(household_code, panel_year, purchase_date)]
  fullData[, "index" := X / Xbar]
  fwrite(fullData, paste0("/home/mallick/Desktop/Nielsen/Data/Clean/FullData/fullData", yr, ".csv"))
}

walk(yr, combineDat)
