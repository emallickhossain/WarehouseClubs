# Gets spending by store type
library(data.table)
library(purrr)
library(stringr)
yrs <- 2004:2017
path <- "/scratch/upenn/hossaine/nielsen_extracts/HMS/"
threads <- 8

retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv", nThread = threads)

fullData <- NULL
for (yr in yrs) {
  print(yr)
  # Getting purchases and combining the same products purchased on the same trip
  purch <- fread(paste0(path, yr, "/Annual_Files/purchases_", yr, ".tsv"),
                 nThread = threads, select = c("trip_code_uc", "total_price_paid"))

  purch <- purch[, .(total_price_paid = sum(total_price_paid)), by = .(trip_code_uc)]

  trips <- fread(paste0(path, yr, "/Annual_Files/trips_", yr, ".tsv"),
                 nThread = threads, select = c("trip_code_uc", "household_code",
                                               "panel_year", "retailer_code"))
  finalData <- merge(purch, trips, by = "trip_code_uc")
  finalData <- merge(finalData, retailers, by = "retailer_code")
  finalData <- finalData[, .(spending = sum(total_price_paid)),
                         by = .(household_code, panel_year, channel_type)]
 fullData <- rbindlist(list(fullData, finalData), use.names = TRUE)
}

fullData[, "annualSpend" := sum(spending), by = .(household_code, panel_year)]
fullData[, "share" := spending / annualSpend * 100]
fullData[channel_type %in% c("Bodega", "Service Station", "Convenience Store",
                             "Drug Store", "Gas Mini Mart"),
         "conv" := 1L]
fullData[is.na(conv), "conv" := 0L]
convStores <- fullData[, sum(share), by = .(household_code, panel_year, conv)]
quantile(convStores[conv == 1]$V1, seq(0, 1, 0.1))
