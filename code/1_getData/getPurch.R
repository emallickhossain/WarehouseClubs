# Gets purchases data
# Drops items that have 0 price
# Drops items with a coupon for > 90% off
# Only keeps trips from cleaned trip data
# Drops magnet data purchases
# Keeping households for which a purchase was made for at least 10 months out of the year
library(data.table)
library(purrr)
yr <- 2004:2016
path <- "/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_2004-2016/nielsen_extracts/HMS/"
prod <- fread("/home/mallick/Desktop/Nielsen/Data/Clean/prod.csv",
              select = c("upc", "upc_ver_uc", "department_code"),
              key = c("upc", "upc_ver_uc"))

getPurch <- function(yr) {
  print(yr)
  trips <- fread(paste0("/home/mallick/Desktop/Nielsen/Data/Clean/Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "purchase_date", "household_code", "panel_year"),
                 key = "trip_code_uc")
  purch <- fread(paste0(path, yr, "/Annual_Files/purchases_", yr, ".tsv"),
                 key = "trip_code_uc")
  purch <- purch[total_price_paid > 0]
  purch <- purch[coupon_value < 0.9 * total_price_paid]
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, c("deal_flag_uc") := NULL]
  purch[, "month" := as.integer(substr(purchase_date, 6, 7))]
  purch[, "shopMonths" := uniqueN(month), by = .(household_code, panel_year)]
  purch <- purch[shopMonths >= 10]
  purch[, c("month", "shopMonths") := NULL]
  fwrite(purch, paste0("/home/mallick/Desktop/Nielsen/Data/Clean/Purchases/purchase", yr, ".csv"))
}
walk(yr, getPurch)
