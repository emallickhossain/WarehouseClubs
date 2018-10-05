# Generates unique brands and stores across households
library(data.table)
library(plotly)
library(purrr)
library(furrr)
library(lfe)
plan(multiprocess)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016
modCode <- 7260

prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "product_module_code",
                         "brand_code_uc", "brand_descr"))[product_module_code == modCode]
panel <- fread(paste0(path, "fullPanel.csv"))
retailers <- fread(paste0(path, "retailers.tsv"))

getCounts <- function(yr) {
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("upc", "upc_ver_uc", "trip_code_uc", "quantity"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))

  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year", "retailer_code"))
  purch <- merge(purch, trips, by = "trip_code_uc")
  setkey(purch, household_code, panel_year, product_module_code)
  return(fullData)
}

fullData <- rbindlist(future_map(yr, getCounts))
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
fullData[brand_code_uc == 536746, "brand_code_uc" := paste0(brand_code_uc, channel_type)]

brandCount <- fullData[, .(count = weighted.mean(brandCount, w = projection_factor)),
                       by = .(panel_year, household_income)]
plot_ly(data = brandCount, x = ~panel_year) %>%
  add_lines(y = ~count, split = ~household_income)

reg <- felm(data = fullData, brandCount ~ household_income + storeCount |
              household_size + as.factor(panel_year) + as.factor(marital_status) +
              as.factor(race) + as.factor(hispanic_origin) + market + age +
              as.factor(college) + as.factor(urban) | 0 | market, weights = fullData$projection_factor)

plot_ly(data = fullData, x = ~tripCount) %>%
  add_trace(y = ~storeCount, type = "scatter", mode = "markers")

plot_ly(data = fullData, x = ~tripCount) %>%
  add_trace(y = ~brandCount, type = "scatter", mode = "markers")




fullData[, "market" := as.factor(market)]
fullData[, "cents" := unitCost * 100]
