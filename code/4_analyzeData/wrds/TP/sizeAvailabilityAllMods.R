# Computes size availability of all non-food products in 2016
library(data.table)
library(ggplot2)
library(ggthemes)
yrs <- 2016
threads <- 8
path <- "/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/"
zipImpute <- fread("/scratch/upenn/hossaine/zipImpute.csv",
                   select = c("zipImpute", "store_code_uc"))

# Getting products and upc versions
prod <- fread("/scratch/upenn/hossaine/fullProd.csv", nThread = threads,
              select = c("upc", "upc_ver_uc", "food", "product_module_code",
                         "quintile", "totalAmount"))[food == 0]
prod[, "food" := NULL]
rms <- fread(paste0(path, "Annual_Files/rms_versions_2016.tsv"), drop = "panel_year")

# Getting store and retail types
retailers <- fread("/scratch/upenn/hossaine/fullRetailers.csv")
stores <- fread(paste0(path, "Annual_Files/stores_2016.tsv"),
                select = c("store_code_uc", "retailer_code"))
retailers <- merge(retailers, stores, by = "retailer_code")
fileNames <- list.files("/scratch/upenn/hossaine/nielsen_extracts/RMS/2016/Movement_Files",
                        recursive = TRUE, full.names = TRUE)

# Getting annual selection of products for each store
zipSelection <- NULL
for (i in fileNames) {
  assort <- unique(fread(i, select = c("upc", "store_code_uc"), nThread = threads))
  assort <- merge(assort, rms, by = "upc")
  zipSelection <- rbindlist(list(zipSelection, assort), use.names = TRUE)
}

fullData <- merge(zipSelection, prod, by = c("upc", "upc_ver_uc"))
fullData <- merge(fullData, retailers, by = "store_code_uc")
fullData <- merge(fullData, zipImpute, by = "store_code_uc")

avgQuintileChannel <- fullData[, .(avgQtile = mean(quintile)),
                               by = .(zip_code = zipImpute, channel_type)]
avgQuintileMod <- fullData[, .(avgSize = mean(totalAmount)),
                           by = .(zip_code = zipImpute, channel_type,
                                  product_module_code)]

# Getting ACS ZIP income (downloaded from AFF)
zipIncome <- fread("/scratch/upenn/hossaine/zipIncome.csv",
                   select = c("NAME", "S1901_C01_012E"))
zipIncome <- zipIncome[-1]
setnames(zipIncome, c("zip_code", "medianInc"))
zipIncome[, "zip_code" := as.integer(gsub("ZCTA5 ", "", zip_code))]
zipIncome[, "medianInc" := as.integer(medianInc)]

# Merging with avg package quintile selection
avgQuintileChannel <- merge(avgQuintileChannel, zipIncome, by = "zip_code")
avgQuintileChannel[, "incBin" := cut(medianInc, breaks = c(seq(0, 90000, 10000), Inf),
                                     labels = seq(10, 100, 10))]
avgQuintileChannel[, "incBin" := as.integer(as.character(incBin))]

avgQuintileMod <- merge(avgQuintileMod, zipIncome, by = "zip_code")
avgQuintileMod[, "incBin" := cut(medianInc, breaks = c(seq(0, 90000, 10000), Inf),
                                 labels = c(as.character(seq(10, 90, 10)), "100"))]

# Graphing size availability by channel
graphData <- avgQuintileChannel[, .(avgAvail = mean(avgQtile)),
                                keyby = .(incBin, channel_type)]
ggplot(data = na.omit(graphData[channel_type != "Dollar Store"]),
       aes(x = incBin, y = avgAvail, shape = channel_type)) +
  geom_point() +
  geom_hline(yintercept = 2.6) +
  geom_vline(xintercept = 15) +
  scale_x_continuous(breaks = seq(10, 100, 10)) +
  labs(x = "Median Income ($000)",
       y = "Package Size Quintile",
       shape = "Store Type") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_color_grey()
ggsave("./figures/sizeAvailabilityAllModsChannelType.png", height = 4, width = 6)

# Graphing size availability by channel and module
graphData <- avgQuintileMod[, .(avgAvail = mean(avgSize)),
                                keyby = .(incBin, channel_type, product_module_code)]
graphData <- melt(graphData, id.vars = c("incBin", "channel_type"))
ggplot(data = na.omit(graphData), aes(x = incBin, y = avgAvail, shape = channel_type)) +
  geom_point() +
  facet_wrap(vars(product_module_code), scales = "free_y") +
  labs(title = "Average Package Quintile By Zip Code Median Income",
       x = "Median Income ($000)", y = "Package Size Quintile") +
  theme_tufte() +
  theme(axis.title = element_text(),
        plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
