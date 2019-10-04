# Computes lowest price for each non-food product in 2016
library(data.table)
library(lubridate)
library(purrr)
threads <- 8

# Loading data
purch <- fread("/scratch/upenn/hossaine/fullPurch2016.csv", nThread = threads)[food == 0]
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads)[panel_year == 2016]
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads)[panel_year == 2016]

# Combining data together
fullData <- merge(purch, trips, by = "trip_code_uc")
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
fullData <- fullData[, .(household_code, panel_year, upc, upc_ver_uc,
                         quantity, packagePrice, product_module_code,
                         upc_descr, brand_code_uc, brand_descr, totalAmount,
                         retailer_code, store_code_uc, purchase_date,
                         channel_type, projection_factor, household_size,
                         household_income, household_income_coarse, age, adults,
                         nChildren, college, married, law)]
fullData[, "week_end" := as.Date(purchase_date, format = "%Y-%m-%d")]
fullData[, "week_end" := ceiling_date(week_end, "week", week_start = 6,
                                      change_on_boundary = FALSE)]
fullData[, "week_end" := as.integer(gsub("-", "", week_end))]

# Computing total spending before dropping missing stores
fullData[, "annualSpend" := sum(packagePrice * quantity),
         by = .(household_code, panel_year, product_module_code)]
fullData <- fullData[store_code_uc != 0]

# Getting store assortment data for stores and dates in the purchase data
ids <- unique(fullData[, .(store_code_uc, week_end)])

fileNames <- list.files("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/Movement_Files/",
                        recursive = TRUE, full.names = TRUE)

getAssort <- function(fileName) {
  print(fileName)
  data <- fread(fileName)
  data <- merge(data, ids, by = c("store_code_uc", "week_end"))
  return(data)
}

fullAssort <- rbindlist(map(fileNames, getAssort), use.names = TRUE)

