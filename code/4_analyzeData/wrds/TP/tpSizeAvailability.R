# Computes size availability of TP
library(data.table)
library(ggplot2)
library(ggthemes)
yrs <- 2006:2016
threads <- 4
path <- "/home/mallick/Desktop/Nielsen/Data/Scanner/tp/"
zipImpute <- fread("./code/0_data/zipImpute.csv", select = c("zipImpute", "store_code_uc"))

# Getting number of rolls per package
prod <- fread(paste0(path, "Master_Files/Latest/products.tsv"), nThread = threads,
              select = c("upc", "upc_ver_uc", "product_module_code",
                         "multi", "size1_amount", "brand_code_uc"),
              quote = "")[product_module_code == 7260]
prod[, "product_module_code" := NULL]

prod[upc == 1115032045 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 3003402434 & upc_ver_uc == 2, "multi" := 6]
prod[upc == 3040000070 & upc_ver_uc == 1, "size1_amount" := 4]
prod[upc == 3040076541 & upc_ver_uc == 2, "size1_amount" := 1]
prod[upc == 3600010669 & upc_ver_uc == 1, "multi" := 3]
prod[upc == 3600011640 & upc_ver_uc == 2, "multi" := 5]
prod[upc == 3600011643 & upc_ver_uc == 3, "multi" := 6]
prod[upc == 3600041605 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 3600064119 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 3600067652 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 3600067667 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 3600067794 & upc_ver_uc == 1, "multi" := 2]
prod[upc == 3700002045 & upc_ver_uc == 1, "multi" := 2]
prod[upc == 3700002072 & upc_ver_uc == 2, "multi" := 6]
prod[upc == 3700006470 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 3700006474 & upc_ver_uc %in% 1:2, "multi" := 9]
prod[upc == 3700012387 & upc_ver_uc %in% 1:3, c("multi", "size1_amount") := .(6, 4)]
prod[upc == 3700024064 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 3700029315 & upc_ver_uc == 2, "size1_amount" := 24]
prod[upc == 3700029319 & upc_ver_uc == 3, "multi" := 4]
prod[upc == 3700032666 & upc_ver_uc == 2, "multi" := 4]
prod[upc == 3700034021 & upc_ver_uc == 2, "size1_amount" := 6]
prod[upc == 3700034030 & upc_ver_uc == 2, c("multi", "size1_amount") := .(6, 6)]
prod[upc == 3700044975 & upc_ver_uc == 1, "multi" := 2]
prod[upc == 3700046162 & upc_ver_uc == 1, "multi" := 4]
prod[upc == 3700046821 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 3700047936 & upc_ver_uc == 2, "multi" := 9]
prod[upc == 3700083747 & upc_ver_uc == 1, "multi" := 4]
prod[upc == 3700083752 & upc_ver_uc == 2, "size1_amount" := 4]
prod[upc == 4116344619 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 4116344624 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 4116344628 & upc_ver_uc == 1, "multi" := 1]
prod[upc == 4127079116 & upc_ver_uc == 1, "size1_amount" := 12]
prod[upc == 4200070066 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 4200086237 & upc_ver_uc == 1, "multi" := 5]
prod[upc == 4200086510 & upc_ver_uc %in% 1:2, "multi" := 6]
prod[upc == 4200087146 & upc_ver_uc == 2, "size1_amount" := 9]
prod[upc == 4200096516 & upc_ver_uc == 1, "multi" := 4]
prod[upc == 4200096517 & upc_ver_uc == 1, "multi" := 4]
prod[upc == 4218739989 & upc_ver_uc == 1, "size1_amount" := 6]
prod[upc == 4218739989 & upc_ver_uc == 2, c("multi", "size1_amount") := .(6, 4)]
prod[upc == 4303202208 & upc_ver_uc == 2, "multi" := 1]
prod[upc == 4303202208 & upc_ver_uc == 3, "size1_amount" := 20]
prod[upc == 4303203408 & upc_ver_uc == 1, "size1_amount" := 1]
prod[upc == 5400000005 & upc_ver_uc == 1, "size1_amount" := 2]
prod[upc == 5400000009 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 5400000010 & upc_ver_uc %in% 1:2, c("multi", "size1_amount") := .(1, 16)]
prod[upc == 5400041210 & upc_ver_uc == 1, "multi" := 20]
prod[upc == 5400042320 & upc_ver_uc %in% 1:2, "multi" := 20]
prod[upc == 5400042330 & upc_ver_uc == 2, "multi" := 30]
prod[upc == 6132835112 & upc_ver_uc == 1, "size1_amount" := 12]
prod[upc == 61429940221 & upc_ver_uc == 2, "size1_amount" := 4]
prod[upc == 7200015561 & upc_ver_uc == 1, "multi" := 1]
prod[upc == 7417505815 & upc_ver_uc == 1, "size1_amount" := 6]
prod[upc == 7417505831 & upc_ver_uc == 1, "size1_amount" := 24]
prod[upc == 7545007963 & upc_ver_uc == 2, "multi" := 1]
prod[upc == 7789022903 & upc_ver_uc == 1, "multi" := 1]
prod[upc == 7874201206 & upc_ver_uc == 2, "size1_amount" := 4]
prod[upc == 88867001012 & upc_ver_uc == 2, "multi" := 1]
prod[upc == 9661915988 & upc_ver_uc == 1, "multi" := 6]
prod[upc == 9661915988 & upc_ver_uc == 2, "size1_amount" := 6]
prod[upc == 18368900019 & upc_ver_uc == 2, "size1_amount" := 24]
prod[upc == 68113172120 & upc_ver_uc == 2, "multi" := 4]
prod[upc == 68826712833 & upc_ver_uc == 1, "size1_amount" := 4]
prod[upc == 71754411316 & upc_ver_uc == 2, "multi" := 1]
prod[upc == 72645900024 & upc_ver_uc == 2, "size1_amount" := 24]
prod[upc == 72796901557 & upc_ver_uc == 1, "size1_amount" := 12]

prod[, "rolls" := multi * size1_amount][, c("multi", "size1_amount") := NULL]
prod <- prod[rolls < 96]

# Getting annual selection of products for each store
zipSelection <- NULL
for (yr in yrs) {
  tp <- fread(paste0(path, yr, "/Movement_Files/4507_", yr, "/7260_", yr, ".tsv"),
              nThread = threads, select = c("store_code_uc", "upc", "units", "price"))
  upcVer <- fread(paste0(path, yr, "/Annual_Files/rms_versions_", yr, ".tsv"))
  tp <- merge(tp, upcVer, by = "upc")
  tp <- merge(tp, prod, by = c("upc", "upc_ver_uc"))
  tp[, c("upc", "upc_ver_uc", "price", "brand_code_uc") := NULL]

  # Merging with ZIP code
  tp <- merge(tp, zipImpute, by = "store_code_uc")
  tpFull <- tp[, .(avgRollsSales = weighted.mean(rolls, w = units),
                   avgRollsAvail = mean(rolls),
                   units = sum(units)),
                     by = .(zip_code = zipImpute, panel_year)]
  zipSelection <- rbindlist(list(zipSelection, tpFull), use.names = TRUE)
}

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

# Graphing size availability
graphData <- zipSelection[, .(avgRollsSales = weighted.mean(avgRollsSales, w = units),
                              avgRollsAvail = mean(avgRollsAvail)),
                          keyby = .(incBin)]
graphData <- melt(graphData, id.vars = c("incBin"))
setnames(graphData, "variable", "Weight")
graphData[Weight == "avgRollsSales", "Weight" := "Sales"]
graphData[Weight == "avgRollsAvail", "Weight" := "Equal"]
ggplot(data = na.omit(graphData),
       aes(x = incBin, y = value, color = Weight)) +
  geom_point() +
  labs(title = "Average Package Size Increases In Median Income",
       x = "Median Income ($000)", y = "Rolls per Package",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
ggsave("./code/5_figures/tpSizeAvailability.png", width = 8, height = 5)

# Graphing unit prices by brand and income
graphData <- zipSelection[, .(unitPriceSales = weighted.mean(unitPriceSales, w = unitsRolls),
                              unitPriceAvail = mean(unitPriceAvail),
                              units = sum(units)),
                          keyby = .(incBin, brand_code_uc, dma_code)]
graphData <- na.omit(melt(graphData,
                          id.vars = c("incBin", "brand_code_uc", "units", "dma_code")))
setnames(graphData, "variable", "Weight")
graphData[Weight == "unitPriceSales", "Weight" := "Sales"]
graphData[Weight == "unitPriceAvail", "Weight" := "Equal"]
ggplot(data = graphData[units > 250000], aes(x = incBin, y = value, color = Weight)) +
  geom_point() +
  facet_grid(cols = vars(dma_code), rows = vars(brand_code_uc)) +
  labs(title = "Average Package Size Increases In Median Income",
       x = "Median Income ($000)", y = "Rolls per Package",
       caption = paste0("Source: Author calulations from Nielsen Consumer Panel.")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_color_grey()
