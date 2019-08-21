# Gets spending shares by channel and storability
library(data.table)
library(ggthemes)
library(ggplot2)
library(lfe)
library(stringr)
yrs <- 2004:2017
threads <- 8
stores <- c("Discount Store", "Warehouse Club", "Grocery")

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
                 select = c("trip_code_uc", "packagePrice", "quantity",
                            "quintile", "product_module_code", "food"),
                 key = "trip_code_uc")
  purch[, "spend" := quantity * packagePrice]
  purch[, c("quantity", "packagePrice") := NULL]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

fullPurch <- merge(fullPurch, trips, by = "trip_code_uc")
fullPurch[, "bulk" := as.integer(quintile >= 4)]
annualSpend <- fullPurch[, .(spend = sum(spend)),
                         by = .(household_code, panel_year, channel_type,
                                food, bulk, quintile)]
annualSpend[, "totalExp" := sum(spend), by = .(household_code, panel_year)]
annualSpend[, "foodExp" := sum(spend), by = .(household_code, panel_year, food)]
annualSpend[, "bulkExp" := sum(spend), by = .(household_code, panel_year, bulk)]
annualSpend[, "channelFoodExp" := sum(spend),
            by = .(household_code, panel_year, channel_type, food)]

############## COMPUTING CHANNEL SHARES OF TOTAL EXPENDITURES ##################
channelShare <- annualSpend[, .(spend = sum(spend),
                                totalExp = mean(totalExp)),
                            keyby = .(household_code, panel_year, channel_type)]

# Adding in zeros
zeros <- unique(channelShare[, .(id = paste(household_code, panel_year, sep = "_"))])
zeros <- as.data.table(expand.grid(id = zeros$id, channel_type = stores))
zeros[, c("household_code", "panel_year") := tstrsplit(id, "_")]
zeros[, c("household_code", "panel_year", "id") :=
        .(as.integer(household_code), as.integer(panel_year), NULL)]
channelShare <- merge(zeros, channelShare, by = c("household_code", "panel_year",
                                                  "channel_type"), all = TRUE)
channelShare[is.na(spend), "spend" := 0]
channelShare[, "totalExp" := mean(totalExp, na.rm = TRUE), by = .(household_code, panel_year)]

# Adding in household characteristics
channelShare <- merge(channelShare, panel, by = c("household_code", "panel_year"))
channelShare <- channelShare[channel_type %in% stores]

# Graphing
graphData <- channelShare[, .(spend = weighted.mean(spend, w = projection_factor),
                              totalExp = weighted.mean(totalExp, w = projection_factor)),
                          by = .(household_income, channel_type, panel_year)]
graphData <- graphData[, .(share = mean(spend / totalExp)),
                       by = .(household_income, channel_type)]
graphData[, "household_income" := factor(household_income,
                                         levels = c(4, 6, 8, 10, 11, 13, 15, 16, 17,
                                                    18, 19, 21, 23, 26, 27),
                                         labels = c(6.5, 9, 11, 13.5, 17.5, 22.5, 27.5,
                                                    32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                                         ordered = TRUE)]
otherStores <- graphData[, .(share = 1 - sum(share),
                             channel_type = "Other"),
                         by = .(household_income)]
graphData <- rbindlist(list(graphData, otherStores), use.names = TRUE)
ggplot(data = na.omit(graphData),
       aes(x = as.numeric(as.character(household_income)),
           y = share * 100, color = channel_type)) +
  geom_point() +
  geom_point(aes(shape = channel_type), size = 3) +
  geom_hline(yintercept = 0) +
  labs(x = "Household Income",
       y = "Percent of Annual Spending",
       shape = "Store Type",
       color = "Store Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()
ggsave("./figures/expendituresByChannel.png", height = 4, width = 6)

############## COMPUTING CHANNEL SHARES OF FOOD/NONFOOD EXPENDITURES ###########
foodShare <- annualSpend[, .(spend = sum(spend),
                             foodExp = mean(foodExp)),
                             keyby = .(household_code, panel_year, food, channel_type)]

# Adding in zeros
zeros <- unique(foodShare[, .(id = paste(household_code, panel_year,
                                         food, sep = "_"))])
zeros <- as.data.table(expand.grid(id = zeros$id, channel_type = stores))
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

# Adding in household characteristics
foodShare <- merge(foodShare, panel, by = c("household_code", "panel_year"))
foodShare <- foodShare[channel_type %in% stores]

# Graphing
graphData <- foodShare[, .(spend = weighted.mean(spend, w = projection_factor),
                               foodExp = weighted.mean(foodExp, w = projection_factor)),
                           by = .(household_income, channel_type, panel_year, food)]
graphData <- graphData[, .(share = mean(spend / foodExp)),
                       by = .(household_income, channel_type, food)]
graphData[, "household_income" := factor(household_income,
                                         levels = c(8, 10, 11, 13, 15, 16, 17,
                                                    18, 19, 21, 23, 26, 27),
                                         labels = c(11, 13.5, 17.5, 22.5, 27.5,
                                                    32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                                         ordered = TRUE)]
otherStores <- graphData[, .(share = 1 - sum(share),
                             channel_type = "Other"),
                         by = .(household_income, food)]
graphData <- rbindlist(list(graphData, otherStores), use.names = TRUE)
graphData[, "foodC" := ifelse(food == 1, "Food", "Non-Food")]
ggplot(data = na.omit(graphData),
       aes(x = as.numeric(as.character(household_income)),
           y = share * 100, color = channel_type)) +
  geom_point() +
  geom_point(aes(shape = channel_type), size = 3) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(foodC)) +
  labs(title = "Perishables Are Purchased at Grocery Stores",
       subtitle = "Clubs and Discount Expenditure Shares Do Not Differ by Product",
       x = "Household Income", y = "Percent of Annual Spending",
       shape = "Store Type",
       color = "Store Type",
       caption = paste0("Note: Household income is the midpoint of income bins reported \n",
                        "to Nielsen. 'Other' includes all other stores such as convenience \n",
                        "stores, dollar stores, etc. Non-perishable items are items that \n",
                        "do not deteriorate if kept unopened at room temperature. \n",
                        "Each panel shows share of total perishable or non-perishable expenditures \n",
                        "made at each store type.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
ggsave("./figures/expendituresByChannelStorability.png", height = 6, width = 6)

############## COMPUTING CHANNEL SHARES OF BULK/NONBULK EXPENDITURES ###
bulkShare <- annualSpend[, .(share = sum(spend / bulkExp)),
                             keyby = .(household_code, panel_year, bulk, channel_type)]

# Adding in zeros
zeros <- unique(bulkShare[, .(id = paste(household_code, panel_year,
                                         bulk, sep = "_"))])
zeros <- as.data.table(expand.grid(id = zeros$id, channel_type = stores))
zeros[, c("household_code", "panel_year", "bulk") := tstrsplit(id, "_")]
zeros[, c("household_code", "panel_year", "bulk", "id") :=
        .(as.integer(household_code), as.integer(panel_year),
          as.integer(bulk), NULL)]
bulkShare <- merge(zeros, bulkShare,
                   by = c("household_code", "panel_year",
                          "channel_type", "bulk"), all = TRUE)
bulkShare[is.na(share), "share" := 0]

# Adding in household characteristics
bulkShare <- merge(bulkShare, panel, by = c("household_code", "panel_year"))
bulkShare <- bulkShare[channel_type %in% stores]

# Graphing
graphData <- bulkShare[, .(share = weighted.mean(share, w = projection_factor)),
                       by = .(household_income, channel_type, panel_year, bulk)]
graphData <- graphData[, .(share = mean(share)),
                       by = .(household_income, channel_type, bulk)]
graphData[, "household_income" := factor(household_income,
                                         levels = c(8, 10, 11, 13, 15, 16, 17,
                                                    18, 19, 21, 23, 26, 27),
                                         labels = c(11, 13.5, 17.5, 22.5, 27.5,
                                                    32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                                         ordered = TRUE)]
otherStores <- graphData[, .(share = 1 - sum(share),
                             channel_type = "Other"),
                         by = .(household_income, bulk)]
graphData <- rbindlist(list(graphData, otherStores), use.names = TRUE)
graphData[, "bulkC" := ifelse(bulk == 1, "Bulk", "Non-Bulk")]
ggplot(data = na.omit(graphData),
       aes(x = as.numeric(as.character(household_income)),
           y = share * 100, color = channel_type)) +
  geom_point() +
  geom_point(aes(shape = channel_type), size = 3) +
  facet_wrap(vars(bulkC)) +
  labs(title = "Most Bulk Purchases Are At Grocery Stores",
       subtitle = "Clubs Increase Bulk Share With Income",
       x = "Household Income", y = "Percent of Annual Spending",
       shape = "Store Type",
       color = "Store Type",
       caption = paste0("Note: Household income is the midpoint of income bins reported \n",
                        "to Nielsen. 'Other' includes all other stores such as convenience \n",
                        "stores, dollar stores, etc. 'Bulk' sizes are sizes that are in the \n",
                        "top two quintiles of that product category's size distribution. Each \n",
                        "panel shows share of bulk or non-bulk expenditures made at each store type.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
ggsave("./figures/expendituresByChannelBulk.png", height = 6, width = 6)
