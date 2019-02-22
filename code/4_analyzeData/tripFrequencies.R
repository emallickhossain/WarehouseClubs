# Computes bulk discount by module
library(data.table)
library(purrr)
library(furrr)
library(plotly)
library(lfe)
library(stringr)
library(fredr)
library(Hmisc)
library(stargazer)
fredr_set_key(fredAPI)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016

# top modules
topMod <- fread("./code/0_data/topModules.csv")
setorder(topMod, -pct)
topMod <- unique(topMod[, .(product_module_code, product_module_descr)])[1:20]

stores <- c("Grocery", "Warehouse Club", "Discount Store", "Dollar Store")

pce <- setDT(fredr("PCEPI", observation_start = as.Date("2004-01-01")))[, "series_id" := NULL]
pce[, ':=' (panel_year = as.integer(year(date)),
            month = as.integer(month(date)),
            date = NULL)]
setnames(pce, "value", "pce")

prod <- fread(paste0(path, "prod.csv"), key = c("upc", "upc_ver_uc"),
              select = c("upc", "upc_ver_uc", "product_module_code", "multi",
                         "size1_amount", "brand_code_uc"))[product_module_code %in% topMod$product_module_code]
prod[, c("size", "multi", "size1_amount") := .(multi * size1_amount, NULL, NULL)]
retailers <- fread(paste0(path, "retailers.tsv"))[channel_type %in% stores]
retailers[, "channel_type" := as.factor(channel_type)]
panel <- fread(paste0(path, "fullPanel.csv"))

getSize <- function(yr) {
  print(yr)
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"),
                 select = c("trip_code_uc", "upc", "upc_ver_uc", "quantity", "total_price_paid"),
                 key = c("upc", "upc_ver_uc"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch[, c("units", "size", "quantity") := .(size * quantity, NULL, NULL)]

  purch <- purch[, .(spend = sum(total_price_paid),
                     units = sum(units)),
                 by = .(trip_code_uc, product_module_code)]

  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("household_code", "panel_year", "trip_code_uc", "purchase_date"))
  trips[, ':=' (month = as.integer(substr(purchase_date, 6, 7)),
                purchase_date = NULL)]
  trips <- merge(trips, pce, by = c("panel_year", "month"))

  fullData <- merge(purch, trips, by = "trip_code_uc")
  fullData <- fullData[, .(trips = uniqueN(trip_code_uc),
                           units = sum(units),
                           realSpend = sum(spend / pce * 100)),
                       by = .(household_code, panel_year, month, product_module_code)]
  return(fullData)
}

fullData <- rbindlist(map(yr, getSize))
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))

getFreq <- function(mod) {
  reg <- felm(data = fullData[product_module_code == mod],
              log(spend) ~ as.factor(panel_year) * household_income + household_size +
                as.factor(marital_status) + as.factor(race) + as.factor(hispanic_origin) +
                as.factor(college) + as.factor(urban) |
                market | 0 | market,
              weights = fullData[product_module_code == mod]$projection_factor)
  return(summary(reg))
}
bulkDiscounts <- rbindlist(map(topMod$product_module_code, getDisc))
bulkDiscounts <- merge(bulkDiscounts, topMod, by = "product_module_code")

plot_ly(data = bulkDiscounts, x = ~product_module_descr) %>%
  add_bars(y = ~coefs, marker = list(color = c(rep("red", 4), rep("blue", 16))))

