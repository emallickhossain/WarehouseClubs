# Ranks modules by popularity in Nielsen
# I define popularity as the share of households that purchased in that module
# at least once in a year
library(data.table)
library(purrr)
path <- "/home/mallick/Desktop/Nielsen/Data/Clean/"
yr <- 2004:2016
prod <- fread(paste0(path, "prod.csv"),
              select = c("upc", "upc_ver_uc", "product_module_code", "product_module_descr"))
panel <- fread(paste0(path, "fullPanel.csv"),
               select = c("household_code", "panel_year", "projection_factor"))

getTop <- function(yr) {
  print(yr)
  panelYr <- panel[panel_year == yr]
  purch <- fread(paste0(path, "Purchases/purchase", yr, ".csv"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch <- purch[, .(q = sum(quantity)),
                 by = .(trip_code_uc, product_module_code, product_module_descr)]

  trips <- fread(paste0(path, "Trips/trips", yr, ".csv"),
                 select = c("trip_code_uc", "household_code", "panel_year"))
  fullData <- merge(purch, trips, by = "trip_code_uc")
  fullData <- fullData[, .(q = sum(q)),
                       by = .(household_code, panel_year, product_module_code, product_module_descr)]
  fullData <- merge(fullData, panelYr, by = c("household_code", "panel_year"), all.y = TRUE)
  fullData[, ':=' (buy = ifelse(q > 0, 1, 0),
                   proj = sum(panelYr$projection_factor))]
  top20 <- fullData[, .(share = sum(projection_factor)),
                    by = .(product_module_code, product_module_descr, panel_year, proj)]
  top20[, "pct" := share / proj * 100]
  setorder(top20, -pct)
  return(top20)
}

fullData <- rbindlist(map(yr, getTop))
setorder(fullData, panel_year, -pct)
fwrite(fullData, "./code/0_data/topModules.csv")
