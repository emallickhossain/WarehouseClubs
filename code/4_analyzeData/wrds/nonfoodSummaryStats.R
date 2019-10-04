# Gets budget and channel shares of non-food expenditures
library(data.table)
library(ggthemes)
library(ggplot2)
library(lfe)
library(stringr)
yrs <- 2004:2017
threads <- 8
stores <- c("Discount Store", "Grocery", "Warehouse Club", "Dollar Store", "Drug Store")

# Getting trips and adding in channel type
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("retailer_code", "trip_code_uc", "household_code", "panel_year"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
trips <- merge(trips, retailers, by = "retailer_code")
rm(retailers)

# Getting household characteristics
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child"),
               key = c("household_code", "panel_year"))

# Getting purchases and merging with trips
fullPurch <- NULL
for (yr in 2004:2017) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "packagePrice", "quantity", "food"),
                 key = "trip_code_uc")
  purch[, "spend" := quantity * packagePrice]
  purch[, c("quantity", "packagePrice") := NULL]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

fullPurch <- merge(fullPurch, trips, by = "trip_code_uc")
annualSpend <- fullPurch[, .(spend = sum(spend)),
                         by = .(household_code, panel_year, channel_type, food)]
annualSpend[, "totalExp" := sum(spend), by = .(household_code, panel_year)]
annualSpend[, "foodExp" := sum(spend), by = .(household_code, panel_year, food)]
annualSpend[, "channelFoodExp" := sum(spend),
            by = .(household_code, panel_year, channel_type, food)]

# Getting budget shares of food/nonfood spending (non-food is about 12-13%)
foodBudget <- unique(annualSpend[, .(household_code, panel_year, food, totalExp, foodExp)])
foodBudget <- merge(foodBudget, panel, by = c("household_code", "panel_year"))
avg <- foodBudget[, weighted.mean(foodExp, w = projection_factor),
                  by = .(household_size, food, panel_year)]
avgWide <- dcast(avg, household_size + panel_year ~ food, value.var = "V1")
avgWide[, "share" := `0` / (`0` + `1`)]
avgWide[, mean(share), by = household_size]

############## COMPUTING CHANNEL SHARES OF FOOD/NONFOOD EXPENDITURES ###########
foodShare <- annualSpend[, .(spend = sum(spend),
                             foodExp = mean(foodExp)),
                         keyby = .(household_code, panel_year, food, channel_type)]

# Adding in zeros
zeros <- unique(foodShare[, .(id = paste(household_code, panel_year,
                                         food, sep = "_"))])
zeros <- as.data.table(expand.grid(id = zeros$id, channel_type = unique(foodShare$channel_type)))
zeros[, c("household_code", "panel_year", "food") := tstrsplit(id, "_")]
zeros[, c("household_code", "panel_year", "food", "id") :=
        .(as.integer(household_code), as.integer(panel_year),
          as.integer(food), NULL)]
foodShare <- merge(zeros, foodShare,
                   by = c("household_code", "panel_year", "channel_type",
                          "food"), all = TRUE)
foodShare[is.na(spend), "spend" := 0]
foodShare[, "foodExp" := mean(foodExp, na.rm = TRUE),
          keyby = .(household_code, panel_year, food)]

# Getting budget shares of food/nonfood spending by channel
# Grocery, discount, warehouse, dollar, drug comprise over 90% of spending
foodShare <- merge(foodShare, panel, by = c("household_code", "panel_year"))
channelSpend <- foodShare[, .(spend = weighted.mean(spend, w = projection_factor)),
                          by = .(food, channel_type)]
channelSpend[, "total" := sum(spend), by = food]
channelSpend[, "share" := spend / total]
