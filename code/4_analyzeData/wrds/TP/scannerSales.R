# Look at pricing for TP from scanner data
library(data.table)
library(ggplot2)
library(ggthemes)
pathName <- "/home/mallick/Desktop/Nielsen/Data/Scanner/"
tp <- fread(paste0(pathName, "Movement_Files/7260_2010.tsv"))
stores <- fread(paste0(pathName, "Annual_Files/stores_2010.tsv"),
                 select = c("store_code_uc", "channel_code"))
prod <- fread(paste0(pathName, "products.tsv"), quote = "")[product_module_code == 7260]
prod <- unique(prod, by = "upc") # Taking only the first entry of multi-version UPCs
prod[, "size" := multi * size1_amount]
prod <- prod[, .(upc, brand_code_uc, brand_descr, size)]
fullData <- merge(tp, prod, by = "upc")
fullData <- merge(fullData, stores, by = "store_code_uc")
rm(tp, prod, stores)
fullData[, "sales" := price * units]
fullData[, "unitCost" := price / size]
fullData[, c("feature", "display") := NULL]
fullData[, ':=' (year = substr(week_end, 1, 4),
                 month = substr(week_end, 5, 6),
                 day = substr(week_end, 7, 8))]
fullData[, "date" := as.Date(paste(year, month, day, sep = "-"))]
fullData[, c("week_end", "year", "month", "day") := NULL]
topStores <- fullData[, .(total = sum(sales)), by = .(store_code_uc, channel_code)]
setorder(topStores, -total)


# Looking at prevalence of prmult deals (2 for $5 or other similar deals)
prop.table(table(fullData$prmult)) # 99.9% of sales do not have a mult deal

# Getting mean price by package size
meanPrices <- fullData[, .(price = weighted.mean(price, w = units),
                           unitCost = weighted.mean(unitCost, w = units)),
                       by = .(store_code_uc, brand_code_uc, brand_descr, size, date)]
brands <- c("CHARMIN", "CHARMIN BASIC", "SCOTT 1000", "ANGEL SOFT", "KLEENEX COTTONELLE",
            "QUILTED NORTHERN", "CTL BR", "CHARMIN BASIC", "SCOTT EXTRA SOFT",
            "COTTONELLE", "MARCAL SMALL STEPS")
ggplot(data = meanPrices[brand_descr %in% brands & store_code_uc == 4544550],
       aes(x = date, y = unitCost, color = as.factor(size))) +
  facet_grid(cols = vars(brand_descr)) +
  geom_line() +
  theme_fivethirtyeight()
ggsave(filename = "./code/5_figures/tpScanner4544550.png")

ggplot(data = meanPrices[brand_descr %in% brands & store_code_uc == 1038627],
       aes(x = date, y = unitCost, color = as.factor(size))) +
  facet_grid(cols = vars(brand_descr)) +
  geom_line() +
  theme_fivethirtyeight()
ggsave(filename = "./code/5_figures/tpScanner1038627.png")

ggplot(data = meanPrices[brand_descr %in% brands & store_code_uc == 173082],
       aes(x = date, y = unitCost, color = as.factor(size))) +
  facet_grid(cols = vars(brand_descr)) +
  geom_line() +
  theme_fivethirtyeight()
ggsave(filename = "./code/5_figures/tpScanner173082.png")
