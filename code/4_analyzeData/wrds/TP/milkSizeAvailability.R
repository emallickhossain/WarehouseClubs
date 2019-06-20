# Computes size availability of milk
library(data.table)
library(ggplot2)
library(ggthemes)
yrs <- 2006:2016
threads <- 4
path <- "/home/mallick/Desktop/Nielsen/Data/Scanner/milk/"
zipImpute <- fread("./code/0_data/zipImpute.csv", select = c("zipImpute", "store_code_uc"))

# Getting annual selection of products for each store
fullTP <- NULL
for (yr in yrs) {
  tp <- fread(paste0(path, yr, "/Movement_Files/2506_", yr, "/3625_", yr, ".tsv"),
              nThread = threads, select = c("store_code_uc", "upc", "units"))
  tp <- tp[, .(units = sum(units)), by = .(store_code_uc, upc)]
  upcVer <- fread(paste0(path, yr, "/Annual_Files/rms_versions_", yr, ".tsv"))
  tp <- merge(tp, upcVer, by = "upc")
  stores <- fread(paste0(path, yr, "/Annual_Files/stores_", yr, ".tsv"),
                  select = c("store_code_uc", "channel_code"))
  tp <- merge(tp, stores, by = "store_code_uc")
  fullTP <- rbindlist(list(fullTP, tp), use.names = TRUE)
}

# Getting number of rolls per package
prod <- fread(paste0(path, "Master_Files/Latest/products.tsv"), nThread = threads,
              select = c("upc", "upc_ver_uc", "product_module_code",
                         "multi", "size1_amount"),
              quote = "")[product_module_code == 3625]
prod[, "product_module_code" := NULL]

prod[, "oz" := multi * size1_amount][, c("multi", "size1_amount") := NULL]
prod <- prod[oz <= 256]

# Combining with store selections and ZIP codes
fullTP <- merge(fullTP, prod, by = c("upc", "upc_ver_uc"))
fullTP <- merge(fullTP, zipImpute, by = "store_code_uc")

# Getting average rolls available by ZIP code and store type
zipSelection <- fullTP[, .(avgOzSales = weighted.mean(oz, w = units),
                           avgOzAvail = mean(oz)),
                       by = .(zip_code = zipImpute, channel_code)]

# Getting ACS ZIP income (downloaded from AFF)
zipIncome <- fread("./code/0_data/zipIncome.csv", select = c("NAME", "S1901_C01_012E"))
zipIncome <- zipIncome[-1]
setnames(zipIncome, c("zip_code", "medianInc"))
zipIncome[, "zip_code" := as.integer(gsub("ZCTA5 ", "", zip_code))]
zipIncome[, "medianInc" := as.integer(medianInc)]

# Merging with avg roll selection
zipSelection <- merge(zipSelection, zipIncome, by = "zip_code")
zipSelection[, "incBin" := cut(medianInc, breaks = c(seq(0, 90000, 10000), Inf),
                               labels = c(as.character(seq(10, 90, 10)), "100+"))]
graphData <- zipSelection[, .(avgOzSales = mean(avgOzSales),
                              avgOzAvail = mean(avgOzAvail)),
                          by = .(incBin)]
graphData <- melt(graphData, id.vars = c("incBin"))
setnames(graphData, "variable", "Weight")
graphData[Weight == "avgOzSales", "Weight" := "Sales"]
graphData[Weight == "avgOzAvail", "Weight" := "Equal"]
ggplot(data = na.omit(graphData), aes(x = incBin, y = value, color = Weight)) +
  geom_point() +
  labs(title = "Average Package Size Decreases In Median Income",
       x = "Median Income ($000)", y = "Ounces Package",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
ggsave("./code/5_figures/milkSizeAvailability.png", width = 6, height = 6)

# Getting sales by store type
zipSales <- fullTP[, .(sales = sum(units)), by = .(channel_code, zip_code = zipImpute)]
zipSales <- merge(zipSales, zipIncome, by = "zip_code")
zipSales[, "incBin" := cut(medianInc, breaks = c(seq(0, 90000, 10000), Inf),
                           labels = c(as.character(seq(10, 90, 10)), "100+"))]
zipSales[, "totSales" := sum(as.numeric(sales)), by = .(incBin)]
zipSales[, "salesShare" := sales / totSales]
graphData <- zipSales[, .(share = sum(salesShare)), by = .(channel_code, incBin)]
ggplot(data = na.omit(graphData), aes(x = incBin, y = share, fill = channel_code)) +
  geom_bar(stat='identity') +
  labs(title = "Average Package Size Increases In Median Income",
       x = "Median Income ($000)", y = "Rolls per Package",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
