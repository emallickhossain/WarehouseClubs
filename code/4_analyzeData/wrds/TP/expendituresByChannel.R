# Gets spending shares by channel
library(data.table)
library(ggthemes)
library(ggplot2)
yrs <- 2004:2017
threads <- 8
stores <- c("Discount Store", "Warehouse Club", "Grocery", "Dollar Store",
            "Drug Store", "Convenience Store")

# Getting trips and adding in channel type
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("retailer_code", "trip_code_uc", "household_code", "panel_year"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
trips <- merge(trips, retailers, by = "retailer_code")
rm(retailers)

# Getting TP purchases and merging with trips
fullPurch <- NULL
for (yr in 2004:2017) {
  print(yr)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", yr, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "packagePrice", "quantity"),
                 key = "trip_code_uc")
  purch[, "spend" := quantity * packagePrice]
  purch[, c("quantity", "packagePrice") := NULL]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

fullPurch <- merge(fullPurch, trips, by = "trip_code_uc")
fullPurch[, "annualSpend" := sum(spend), by = .(household_code, panel_year)]
fullPurch[, "share" := spend / annualSpend]
hhShare <- fullPurch[channel_type %in% stores, .(share = sum(share)),
                     by = .(household_code, panel_year, channel_type)]

# Adding in zeros
zeros <- unique(hhShare[, .(id = paste(household_code, panel_year, sep = "_"))])
zeros <- as.data.table(expand.grid(id = zeros$id, channel_type = stores))
zeros[, c("household_code", "panel_year") := tstrsplit(id, "_")]
zeros[, c("household_code", "panel_year", "id") :=
        .(as.integer(household_code), as.integer(panel_year), NULL)]
hhShare <- merge(zeros, hhShare, by = c("household_code", "panel_year", "channel_type"),
                 all = TRUE)
hhShare[is.na(share), "share" := 0]

# Adding in household characteristics
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child"),
               key = c("household_code", "panel_year"))
hhShare <- merge(hhShare, panel, by = c("household_code", "panel_year"))

# Graphing
graphData <- hhShare[, .(share = weighted.mean(share, w = projection_factor)),
                     by = .(household_income, channel_type, panel_year)]
graphData[, "household_income" := factor(household_income,
                                         levels = c(8, 10, 11, 13, 15, 16, 17,
                                                    18, 19, 21, 23, 26, 27),
                                         labels = c(11, 13.5, 17.5, 22.5, 27.5,
                                                    32.5, 37.5, 42.5, 47.5, 55, 65, 85, 100),
                                         ordered = TRUE)]
ggplot(data = graphData, aes(x = as.numeric(as.character(household_income)),
                             y = share, color = as.factor(panel_year))) +
  geom_line() +
  facet_wrap(vars(channel_type)) +
  labs(title = "Most Households Use One or Fewer Discounts",
       x = "Household Income", y = "Share of Purchases") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
ggsave("./figures/expendituresByChannel.png", height = 6, width = 6)
