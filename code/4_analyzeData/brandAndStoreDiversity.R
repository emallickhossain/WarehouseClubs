# Generates scatter of store and brand diversity across households
library(data.table)
library(plotly)
library(purrr)
path <- "/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_2004-2016/nielsen_extracts/HMS/"
yr <- 2004:2016

prod <- fread(paste0(path, "Master_Files/Latest/products.tsv"),
              select = c("upc", "upc_ver_uc", "product_module_code",
                         "brand_code_uc", "brand_descr"), quote = "")

panel <- fread("/home/mallick/Desktop/Nielsen/Data/Clean/fullPanel.csv")
panel[household_income == "<25k", "income" := 1]
panel[household_income == "25-50k", "income" := 2]
panel[household_income == "50-100k", "income" := 3]
panel[household_income == ">100k", "income" := 4]

getCounts <- function(yr) {
  print(yr)
  purch <- fread(paste0(path, yr, "/Annual_Files/purchases_", yr, ".tsv"),
                 select = c("upc", "upc_ver_uc", "trip_code_uc"))
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))

  trips <- fread(paste0(path, yr, "/Annual_Files/trips_", yr, ".tsv"),
                 select = c("trip_code_uc", "household_code", "panel_year", "retailer_code"))
  purch <- merge(purch, trips, by = "trip_code_uc")
  setkey(purch, household_code, panel_year, product_module_code)
  fullData <- purch[, .(tripCount = uniqueN(trip_code_uc),
                        storeCount = uniqueN(retailer_code),
                        brandCount = uniqueN(brand_code_uc)),
                    by = .(household_code, panel_year)]
  return(fullData)
}

fullData <- rbindlist(map(yr, getCounts))

plot_ly(data = fullData, x = ~tripCount) %>%
  add_trace(y = ~storeCount, type = "scatter", mode = "markers")

plot_ly(data = fullData, x = ~tripCount) %>%
  add_trace(y = ~brandCount, type = "scatter", mode = "markers")




fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
retailers <- fread(paste0(path, "Master_Files/Latest/retailers.tsv"))
fullData <- merge(fullData, retailers, by = c("retailer_code"))
fullData[, "market" := as.factor(market)]
fullData[, "cents" := unitCost * 100]
