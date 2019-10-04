# Produces table of Homescan summary statistics
library(data.table)
library(knitr)
library(Hmisc)
library(forcats)
library(ggplot2)
library(ggthemes)
library(stringr)
threads <- 8
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv",
               select = c("projection_factor", "household_income", "panel_year",
                          "age", "college", "household_code", "married",
                          "men", "women", "nChildren"))

panel[, "household_size" := men + women + nChildren]
panel[, "child" := as.integer(nChildren > 0)]
panel[, "household_income" := factor(household_income,
                                     levels = c(3, 4, 6, 8, 10, 11, 13, 15, 16,
                                                17, 18, 19, 21, 23, 26, 27),
                                     labels = c(2.5, 6.5, 9, 11, 13.5, 17.5,
                                                22.5, 27.5, 32.5, 37.5, 42.5,
                                                47.5, 55, 65, 85, 100),
                                     ordered = TRUE)]
panel[, "household_income" := as.numeric(as.character(household_income))]
panel <- panel[!is.na(household_income)]

# Getting number and tenure of households
tenure <- panel[, .(tenure = uniqueN(panel_year)), by = household_code]
tenure[, .(mean(tenure), median(tenure))]
panel[, c("household_code", "panel_year") := NULL]

# Generating summary stats table
summaryMeans <- panel[, lapply(.SD, weighted.mean, w = projection_factor)]
summaryMeans[, "type" := "Mean"]
summarySD <- panel[, lapply(.SD, wtd.var, w = projection_factor)]
summarySD <- sqrt(summarySD)
summarySD[, "type" := "SD"]
summary25 <- panel[, lapply(.SD, wtd.quantile, weights = projection_factor, 0.25)]
summary25[, "type" := "25th Pctile"]
summary75 <- panel[, lapply(.SD, wtd.quantile, weights = projection_factor, 0.75)]
summary75[, "type" := "75th Pctile"]

finalTable <- rbindlist(list(summaryMeans, summarySD, summary25, summary75), use.names = TRUE)
finalTableLong <- melt(finalTable, id.vars = "type")
finalTableWide <- dcast(finalTableLong, variable ~ type)
setnames(finalTableWide, "variable", "Variable")
setcolorder(finalTableWide, c("Variable", "Mean", "SD", "25th Pctile" ,"75th Pctile"))
finalTableWide <- finalTableWide[Variable != "projection_factor"]
finalTableWide[Variable == "household_income", "Variable" := "Household income ($000s)"]
finalTableWide[Variable == "household_size", "Variable" := "Household size"]
finalTableWide[Variable == "age", "Variable" := "Age"]
finalTableWide[Variable == "college", "Variable" := "College Educated"]
finalTableWide[Variable == "child", "Variable" := "Child Present"]
finalTableWide[Variable == "married", "Variable" := "Married"]

kable(finalTableWide, digits = 2, format = "markdown")
# saved as homescanSummaryStats.tex

# Getting top 20 categories by spending in 2017
purch <- fread("/scratch/upenn/hossaine/fullPurch2017.csv", nThread = threads,
               select = c("trip_code_uc", "quantity", "packagePrice",
                          "product_module_code", "totalAmount", "food"))
purch[, "totalSpend" := packagePrice * quantity]
prodSpend <- purch[, .(spend = sum(totalSpend),
                       price = mean(totalSpend),
                       size = mean(totalAmount)), by = .(product_module_code, food)]
setorder(prodSpend, spend)
topMods <- tail(prodSpend, 20)$product_module_code

# Restricting only to purchases of top 20 items
purch <- purch[product_module_code %in% topMods]
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("trip_code_uc", "household_code", "panel_year"))
purch <- merge(purch, trips, by = "trip_code_uc")
panel <- fread("/scratch/upenn/hossaine/fullPanel.csv", nThread = threads,
               select = c("household_code", "panel_year", "projection_factor"))
purch <- merge(purch, panel, by = c("household_code", "panel_year"))

# Getting annual spending for each household, prices paid, and sizes
hhSpending <- purch[, .(spend = sum(totalSpend),
                        price = mean(totalSpend),
                        size = mean(totalAmount)),
                    by = .(household_code, panel_year,
                           projection_factor, product_module_code)]

avgSpending <- hhSpending[, .(spend = weighted.mean(spend, w = projection_factor),
                              spendVar = wtd.var(spend, weights = projection_factor),
                              size = weighted.mean(size, w = projection_factor),
                              sizeVar = wtd.var(size, weights = projection_factor),
                              price = weighted.mean(price, w = projection_factor),
                              priceVar = wtd.var(price, weights = projection_factor)),
                          by = product_module_code]
avgSpending[, c("spendSD", "sizeSD", "priceSD", "spendVar", "sizeVar", "priceVar") :=
              .(sqrt(spendVar), sqrt(sizeVar), sqrt(priceVar), NULL, NULL, NULL)]

prod <- unique(fread(paste0("/scratch/upenn/hossaine/nielsen_extracts/HMS/",
                            "Master_Files/Latest/products.tsv"),
                     select = c("product_module_code", "product_module_descr")))
avgSpending <- merge(avgSpending, prod, by = "product_module_code")
setcolorder(avgSpending, c("product_module_descr", "spend", "spendSD", "price",
            "priceSD", "size", "sizeSD"))
avgSpending[, "product_module_code" := NULL]
setorder(avgSpending, -spend)
setnames(avgSpending, c("Product", "Annual Spending", "SD", "Avg. Price", "SD", "Avg. Size", "SD"))
stargazer(avgSpending, summary = FALSE, type = "text", digits = 2,
          label = "tab:homeScanProducts",
          out = "tables/homeScanProducts.tex",
          rownames = FALSE)

# Get total spending by channel
trips <- fread("/scratch/upenn/hossaine/fullTrips.csv", nThread = threads,
               select = c("channel_type", "trip_code_uc", "panel_year"))

fullPurch <- NULL
for (i in 2004:2017) {
  print(i)
  purch <- fread(paste0("/scratch/upenn/hossaine/fullPurch", i, ".csv"),
                 nThread = threads,
                 select = c("trip_code_uc", "quantity", "packagePrice"))
  purch[, "total" := quantity * packagePrice]
  purch <- purch[, .(total = sum(total)), by = .(trip_code_uc)]
  purch <- merge(purch, trips, by = "trip_code_uc", all.x = TRUE)
  purch[is.na(channel_type), "channel_type" := "Other"]
  purch[is.na(panel_year), "panel_year" := i]
  fullPurch <- rbindlist(list(fullPurch, purch), use.names = TRUE)
}

channelShares <- fullPurch[, .(total = sum(total)), by = .(channel_type, panel_year)]
channelShares[, "share" := total / sum(total), by = panel_year]
channelShares[channel_type == "Other"]
