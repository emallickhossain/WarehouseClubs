# This tests the stocking up hypothesis by comparing the number of trips
# in which each product was purchased. If stocking up exists, then there would
# be fewer trips for storable items and no change for perishables since you
# can't stock up on them by definition
library(data.table)
library(purrr)
library(furrr)
library(plotly)
plan(multiprocess)
yr <- 2004:2016
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
modCode <- 1484

prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "product_module_code", "multi", "size1_amount", "size1_units"),
              key = c("upc", "upc_ver_uc"))
prod[, ':=' (units = multi * size1_amount,
             multi = NULL,
             size1_amount = NULL)]
panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year",
                          "projection_factor", "household_income"))

getTrips <- function(yr){
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("upc", "upc_ver_uc", "trip_code_uc", "quantity", "total_price_paid"),
                 key = c("upc", "upc_ver_uc", "trip_code_uc"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))

  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year", "purchase_date"),
                 key = "trip_code_uc")
  purch <- merge(purch, trips, by = "trip_code_uc")
  final <- purch[, .(trips = uniqueN(trip_code_uc)),
                 by = .(household_code, panel_year, product_module_code)]
  return(final)
}

fullData <- rbindlist(future_map(yr, getTrips))
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))

modTrips <- fullData[, .(trips = weighted.mean(trips, w = projection_factor)),
                     by = .(panel_year, product_module_code, household_income)]
plot_ly(data = modTrips[product_module_code == modCode], x = ~panel_year) %>%
  add_lines(y = ~trips, split = ~household_income)
