# Generates unique brands and stores across households
library(data.table)
library(ggthemes)
library(ggplot2)
library(lfe)
library(Hmisc)
library(knitr)
threads <- 8
yrs <- 2004:2017
path <- "/scratch/upenn/hossaine/"

# Getting data
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year", "retailer_code"))
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
trips <- merge(trips, retailers, by = "retailer_code")

panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("household_code", "panel_year", "projection_factor",
                          "household_income", "household_size", "age", "college",
                          "child", "married", "white"))

fullPurch <- NULL
for (i in yrs) {
  print(i)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", i, ".csv"), nThread = threads,
                 select = c("trip_code_uc", "product_module_code",
                            "brand_code_uc", "food", "packagePrice", "quantity"))
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

fullPurch <- merge(fullPurch, trips, by = "trip_code_uc")
rm(trips)

# Counting unique brands and store types for each household and product
fullPurch[, "totalSpend" := packagePrice * quantity]
fullPurch[, c("packagePrice", "quantity") := NULL]
setkey(fullPurch, household_code, panel_year, product_module_code, food)
uniqueBrands <- fullPurch[, .(brands = uniqueN(brand_code_uc),
                              retailers = uniqueN(retailer_code),
                              channels = uniqueN(channel_type),
                              trips = uniqueN(trip_code_uc),
                              spending = sum(totalSpend)),
                          by = .(household_code, panel_year, product_module_code, food)]
uniqueBrands <- merge(uniqueBrands, panel, by = c("household_code", "panel_year"))

# Taking expenditure weighted average across households (all products)
hhBrands <- uniqueBrands[, .(brands = weighted.mean(brands, w = spending),
                             retailers = weighted.mean(retailers, w = spending),
                             channels = weighted.mean(channels, w = spending)),
                         by = .(household_code, panel_year, projection_factor)]

hhQtile <- hhBrands[, .(Brands = wtd.quantile(brands, weights = projection_factor,
                                              probs = c(0.25, 0.5, 0.75)),
                        Retailers = wtd.quantile(retailers, weights = projection_factor,
                                                 probs = c(0.25, 0.5, 0.75)),
                        Channels = wtd.quantile(channels, weights = projection_factor,
                                                probs = c(0.25, 0.5, 0.75)))]
hhQtile[, "qtile" := c("25th", "Median", "75th")]
hhQtileWide <- dcast(melt(hhQtile, id.vars = c("qtile")), variable ~ qtile)
setnames(hhQtileWide, "variable", "Variable")
setcolorder(hhQtileWide, c("Variable", "25th", "Median", "75th"))
kable(hhQtileWide, format = "markdown", digits = 2)

# Taking expenditure weighted average across households (compare food and non-food purchases)
hhBrands <- uniqueBrands[, .(brands = weighted.mean(brands, w = spending),
                             retailers = weighted.mean(retailers, w = spending),
                             channels = weighted.mean(channels, w = spending)),
                         by = .(household_code, panel_year, projection_factor, food)]

hhQtile <- hhBrands[, .(Brands = wtd.quantile(brands, weights = projection_factor,
                                               probs = c(0.25, 0.5, 0.75)),
                         Retailers = wtd.quantile(retailers, weights = projection_factor,
                                                  probs = c(0.25, 0.5, 0.75)),
                         Channels = wtd.quantile(channels, weights = projection_factor,
                                                 probs = c(0.25, 0.5, 0.75))),
                    by = food]
hhQtile[, "qtile" := c("25th", "Median", "75th")]
hhQtileWide <- dcast(melt(hhQtile, id.vars = c("qtile", "food")), variable + food ~ qtile)
setnames(hhQtileWide, "variable", "Variable")
setcolorder(hhQtileWide, c("Variable", "25th", "Median", "75th"))
setorder(hhQtileWide, food, Variable)
kable(hhQtileWide, format = "latex", digits = 2)
# Saved in brandAndStoreDiversity.tex

# Taking expenditure weighted average across households
hhQtile <- uniqueBrands[, .(Brands = wtd.quantile(brands, weights = projection_factor,
                                                  probs = 0.75),
                            Retailers = wtd.quantile(retailers, weights = projection_factor,
                                                     probs = 0.75),
                            Channels = wtd.quantile(channels, weights = projection_factor,
                                                    probs = 0.75),
                            Trips = wtd.mean(trips, weights = projection_factor)),
                        by = product_module_code]
setorder(hhQtile, Brands)
tail(hhQtile, 20)
