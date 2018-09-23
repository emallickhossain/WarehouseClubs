# Generates scatter of store diversity across households for given trips
library(data.table)
library(plotly)
library(purrr)
path <- "/home/mallick/Desktop/Nielsen/Data/Consumer_Panel_Data_2004-2016/nielsen_extracts/HMS/"
yr <- 2004:2016

panel <- fread("/home/mallick/Desktop/Nielsen/Data/Clean/fullPanel.csv")
panel[household_income == "<25k", "income" := 1]
panel[household_income == "25-50k", "income" := 2]
panel[household_income == "50-100k", "income" := 3]
panel[household_income == ">100k", "income" := 4]

retailers <- fread(paste0(path, "Master_Files/Latest/retailers.tsv"))

getCounts <- function(yr) {
  print(yr)
  purch <- fread(paste0(path, yr, "/Annual_Files/purchases_", yr, ".tsv"),
                 select = c("trip_code_uc", "total_price_paid"))
  purch <- purch[, .(p = sum(total_price_paid)), by = trip_code_uc]

  trips <- fread(paste0(path, yr, "/Annual_Files/trips_", yr, ".tsv"),
                 select = c("trip_code_uc", "household_code", "panel_year",
                            "retailer_code"))
  purch <- merge(purch, trips, by = "trip_code_uc")
  setkey(purch, household_code, panel_year)
  purch[, "tripCount" := uniqueN(trip_code_uc),
        by = .(household_code, panel_year)]
  fullData <- purch[, .(spend = sum(p)),
                    by = .(household_code, panel_year, tripCount, retailer_code)]
  return(fullData)
}

fullData <- rbindlist(map(yr, getCounts))
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
fullData[, "totalSpend" := sum(spend), by = .(household_code, panel_year)]
fullData[, "spendShare" := spend / totalSpend]
fullData[, "hhi" := sum(spendShare ^ 2), by = .(household_code, panel_year)]
fullData <- merge(fullData, panel, by = c("household_code", "panel_year"))
fullData <- merge(fullData, retailers, by = c("retailer_code"))

channelShares <- fullData[, .(share = sum(spendShare)),
                          by = .(household_code, panel_year, projection_factor, channel_type)]
channelShares <- channelShares[, .(share = weighted.mean(share, w = projection_factor)),
                               by = .(channel_type)]

scatterData <- fullData[, .(storeCount = uniqueN(retailer_code)),
                        by = .(household_code, panel_year, tripCount)]

reg <- lm(data = scatterData, storeCount ~ tripCount)
chart <- plot_ly(data = scatterData, x = ~tripCount) %>%
  add_trace(y = ~storeCount, type = "scatter", mode = "markers") %>%
  add_lines(y = fitted(reg))

