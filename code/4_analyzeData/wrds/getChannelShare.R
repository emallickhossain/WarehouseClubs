# Computes share spending by retailer type
library(data.table)
library(purrr)
path <- "/scratch/upenn/hossaine/"
yr <- 2004:2016
panel <- fread(paste0(path, "fullPanel.csv"))
retailer <- fread(paste0(path, "retailers.tsv"))
stores <- c("Dollar Store", "Grocery", "Warehouse Club", "Convenience Store", "Discount Store", "Drug Store")

getData <- function(yr) {
  # Getting Nielsen purchases by trip
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("trip_code_uc", "total_price_paid"))
  purch <- purch[, .(spend = sum(total_price_paid)), by = .(trip_code_uc)]

  # Merging trips
  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year", "retailer_code"))
  purch <- merge(trips, purch, by = "trip_code_uc")
  purch <- merge(purch, retailer, by = "retailer_code")
  purch[, c("trip_code_uc", "retailer_code") := NULL]

  finalData <- purch[, .(spend = sum(spend)),
                     by = .(household_code, panel_year, channel_type)]
  return(finalData)
}

fullData <- rbindlist(map(yr, getData))
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
fullData[, "channel_type" := ifelse(channel_type %in% stores, channel_type, "Other")]

averages <- fullData[, .(spend = weighted.mean(spend, w = projection_factor)),
                     by = .(panel_year, channel_type, household_income)]
averages[, "total" := sum(spend), by = .(panel_year, household_income)]
averages[, "share" := spend / total * 100]

summary(lm(data = averages, share ~ household_income * channel_type))
