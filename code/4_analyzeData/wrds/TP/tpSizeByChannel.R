# Gets TP package size purchased by retailer type
# Fact 1: TP size preferences by channel do not seem to have much difference, but
# this is probably because this is conditional on making a purchase in that channel
# Fact 2: Store choice has a huge effect. There's a clear association in that
# rich households shop at warehouse clubs and poor households shop at dollar
# and discount stores. Everyone shops at grocery stores.
library(data.table)
library(ggthemes)
library(ggplot2)
yrs <- 2004:2017
threads <- 8

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
                 select = c("trip_code_uc", "product_module_code", "totalAmount"),
                 key = "trip_code_uc")[product_module_code == 7260]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

fullPurch <- merge(fullPurch, trips, by = "trip_code_uc")

# Adding in household characteristics
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("panel_year", "household_code", "projection_factor",
                          "household_income", "household_size", "age", "child"),
               key = c("household_code", "panel_year"))
fullPurch <- merge(fullPurch, panel, by = c("household_code", "panel_year"))
fullPurch[, "household_income" := as.factor(household_income)]

# Regressing size purchased
stores <- c("Discount Store", "Warehouse Club", "Grocery", "Dollar Store", "Drug Store",
            "Convenience Store")
reg <- lm(totalAmount ~ household_income * channel_type + household_size +
            age + child, weights = projection_factor,
          data = fullPurch[channel_type %in% stores &
                             household_income %in% paste(8:30)])
coefs <- as.data.table(summary(reg)$coefficients, keep.rownames = TRUE)
coefs[, c("Income", "Store") := tstrsplit(rn, ":")]
coefs[, "Income" := gsub("household_income", "", Income)]
coefs[, "Store" := gsub("channel_type", "", Store)]

# Graphing
graphData <- coefs[Income %in% paste(1:30)]
graphData[is.na(Store), "Store" := "Convenience Store"]
graphData[, "Income" := as.integer(Income)]
setnames(graphData, c("rn", "beta", "se", "t", "p", "Income", "Store"))
ggplot(data = graphData, aes(x = Income, y = beta)) +
  geom_errorbar(aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se), width = 0.2) +
  facet_wrap(vars(Store)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(title = "Most Households Use One or Fewer Discounts",
       x = "Household Income", y = "Share of Purchases",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel. \n",
                        "Note: Demographic adjustments control for household size, \n",
                        "age, and presence of children. Income-specific time trends \n",
                        "are also controlled for.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()

# Total Amount by channel type
fullPurch[, "annualPurch" := sum(totalAmount), by = .(household_code, panel_year)]
fullPurch[, "share" := totalAmount / annualPurch]
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
hhShare <- merge(hhShare, panel, by = c("household_code", "panel_year"))

graphData <- hhShare[, .(share = weighted.mean(share, w = projection_factor)),
                     by = .(household_income, channel_type)]
ggplot(data = graphData, aes(x = household_income, y = share, color = channel_type)) +
  geom_line() +
  theme_fivethirtyeight()
