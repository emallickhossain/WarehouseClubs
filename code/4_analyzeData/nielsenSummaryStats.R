library(data.table)
library(purrr)
library(lubridate)
years <- 2004:2016

# Getting shopping trips
getTrips <- function(year) {
  ans <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_",
                      "2004-2016/nielsen_extracts/HMS/", year,
                      "/Annual_Files/trips_", year, ".tsv"))
  return(ans)
}

trips <- rbindlist(map(years, getTrips), use.names = TRUE, fill = TRUE)
trips[, "purchase_date" := parse_date_time(purchase_date, "%Y-%m-%d")]
trips[, c("year", "month") := .(year(purchase_date), month(purchase_date))]
monthlyTrips <- unique(trips, by = c("household_code", "panel_year", "year", "month"))
monthlyTrips <- monthlyTrips[, .(monthsActive = .N), by = .(household_code, panel_year)]
table(monthlyTrips$monthsActive)

hhCount <- trips[, .(hh = uniqueN(household_code)), by = panel_year]
tenure <- trips[, .(tenure = uniqueN(panel_year)), by = household_code]
tenure <- tenure[, .(mean = mean(tenure), median = median(tenure))]
