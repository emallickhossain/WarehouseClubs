# Estimates how much households could save if they purchased at the lowest
# unit cost to varying degrees of conservativeness
# Because the Scanner coverage was not good for toilet paper, an alternative approach
# is to compute savings based on what people actually purchased. The assumption
# is that if people purchased at a lower unit price either at the same retailer,
# same channel, or same market, then it was available to other
# households in that same place and hence was a "missed" savings opportunity.
# I compute this using the following steps:
# Step 1: For each retailer/channel/market-brand, find the average
# unit price for an item in the largest quantile of the size distribution
# Step 2: Compute potential savings compared to what was actually purchased
# Step 3: Compute expenditure-weighted average savings for each household
# Step 4: Compute projection-weighted average savings for each income group
library(data.table)
library(lfe)
library(purrr)
library(ggplot2)
library(ggthemes)
threads <- 8
stores <- c("Grocery", "Warehouse Club", "Dollar Store", "Discount Store", "Drug Store")

# Getting trips and panel data for demographics and to deflate expenditures
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year", "retailer_code"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
trips <- merge(retailers, trips, by = "retailer_code")[channel_type %in% stores]

# Identifying lowest unit cost item purchased by each household
missedSavingsAll <- NULL
for (yr in 2004:2017) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "brand_code_uc", "product_module_code",
                            "packagePrice", "quantity", "totalAmount"),
                 key = "trip_code_uc")[product_module_code == 7260]
  purch[, ':=' (unitPrice = packagePrice / totalAmount,
                totalExpenditure = packagePrice * quantity)]
  purch[, c("packagePrice", "quantity", "product_module_code") := NULL]

  purch <- merge(purch, trips, by = "trip_code_uc")
  purch[, "minRetail" := min(unitPrice), by = .(channel_type, retailer_code,
                                                brand_code_uc)]
  minRetailSize <- unique(purch[unitPrice == minRetail,
                                .(channel_type, retailer_code, brand_code_uc,
                                  minRetailSize = totalAmount)])
  purch <- merge(purch, minRetailSize, by = c("channel_type", "retailer_code", "brand_code_uc"))
  purch[, "minChannel" := min(unitPrice), by = .(channel_type, brand_code_uc)]
  minChannelSize <- unique(purch[unitPrice == minChannel,
                                 .(channel_type, brand_code_uc,
                                   minChannelSize = totalAmount)])
  minChannelSize[, "size" := max(minChannelSize), by = .(channel_type, brand_code_uc)]
  minChannelSize[, "minChannelSize" := NULL]
  setnames(minChannelSize, "size", "minChannelSize")
  purch <- merge(purch, unique(minChannelSize), by = c("channel_type", "brand_code_uc"))

  # Savings
  purch[, "retailSavings" := ((unitPrice - minRetail) / unitPrice) * (totalAmount < minRetailSize)]

  # Adding in HH market
  panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
                 select = c("panel_year", "household_code", "market"),
                 key = c("household_code", "panel_year"))
  purch <- merge(purch, panel, by = c("household_code", "panel_year"))

  # Getting minimum prices for each level of specificity.
  # I get the mean unit price for the largest quartile offered by each retailer
  # Then by channel
  # Then by market
  minPriceRetail <- purch[totalAmount == maxQRetail,
                         .(retailMin = mean(realUnitPrice)),
                         by = .(panel_year, market, channel_type, retailer_code,
                                product_module_code, brand_code_uc)]
  minPriceChannel <- purch[totalAmount == maxQChannel,
                          .(channelMin = mean(realUnitPrice)),
                          by = .(panel_year, market, channel_type,
                                 product_module_code, brand_code_uc)]
  minPriceMarket <- purch[totalAmount == maxQMarket,
                          .(marketMin = mean(realUnitPrice)),
                           by = .(panel_year, market, product_module_code, brand_code_uc)]

  # Merging with purchase data
  purch <- merge(purch, minPriceRetail, all.x = TRUE,
                 by = c("panel_year", "market", "channel_type", "retailer_code",
                        "product_module_code", "brand_code_uc"))
  purch <- merge(purch, minPriceChannel, all.x = TRUE,
                 by = c("panel_year", "market", "channel_type",
                        "product_module_code", "brand_code_uc"))
  purch <- merge(purch, minPriceMarket, all.x = TRUE,
                 by = c("panel_year", "market", "product_module_code", "brand_code_uc"))

  # Adjusting for generic brands since they don't have channel or market level equivalents
  purch[brand_code_uc == 536746, c("channelMin", "marketMin") := .(retailMin, retailMin)]

  # Getting "missed savings"
  purch[, "savingsRetailer" := (realUnitPrice - retailMin) / realUnitPrice]
  purch[, "savingsChannel" := (realUnitPrice - channelMin) / realUnitPrice]
  purch[, "savingsMarket" := (realUnitPrice - marketMin) / realUnitPrice]

  # If "missed savings" are negative, then adjust to 0 because there's nothing they would miss
  purch[savingsRetailer < 0, "savingsRetailer" := 0]
  purch[savingsChannel < 0, "savingsChannel" := 0]
  purch[savingsMarket < 0, "savingsMarket" := 0]

  # Computing savings
  cols <- c("savingsRetailer", "savingsChannel", "savingsMarket")
  purch <- purch[market != "Remaining US"]
  missedSavings <- purch[, lapply(.SD, weighted.mean, w = realExp, na.rm = TRUE),
                         by = .(household_code, panel_year), .SDcols = cols]

  # Combining together
  missedSavingsAll <- rbindlist(list(missedSavingsAll, missedSavings), use.names = TRUE)
}

# Adding demographics
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child",
                          "market"),
               key = c("household_code", "panel_year"))
panel[, "household_income" := as.factor(household_income)]
missedSavingsAll <- merge(missedSavingsAll, panel, by = c("household_code", "panel_year"))

# Running regressions
runRegAll <- function(y) {
  regForm <- as.formula(paste0(y, "~", "household_income +",
                               "+ age + household_size + child + 0"))

  # All products
  regData <- missedSavingsAll
  regAll <- felm(data = regData, formula = regForm, weights = regData$projection_factor)
  coefsAll <- as.data.table(summary(regAll)$coefficients, keep.rownames = TRUE)
  coefsAll[, "savings" := y]
  return(coefsAll)
}

discounts <- c("savingsRetailer", "savingsChannel", "savingsMarket")
finalCoefs <- rbindlist(map(discounts, runRegAll), use.names = TRUE)

# Organizing graph data
graphData <- finalCoefs[grepl("household_income", rn)]
graphData[, "rn" := gsub("household_income", "", rn)]
graphData[, "rn" := factor(rn, levels = c(8, 10, 11, 13, 15, 16, 17, 18, 19, 21, 23, 26, 27),
                           labels = c(11, 13.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                           ordered = TRUE)]
graphData[, "rn" := as.numeric(as.character(rn))]
setnames(graphData, c("rn", "beta", "se", "t", "p", "Savings"))

# Graphing
ggplot(data = graphData, aes(x = rn, y = beta)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(Savings)) +
  labs(title = "Rich Households Bulk Buy More",
       x = "Household Income", y = "Share of Purchases",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, and presence of children. Income-specific time trends \n",
                        "are also controlled for.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
