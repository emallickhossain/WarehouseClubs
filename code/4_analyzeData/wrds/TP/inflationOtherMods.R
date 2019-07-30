# Generates price series for other modules
library(data.table)
library(stringr)
library(ggplot2)
library(ggthemes)
yrs <- 2006:2016
threads <- 8
moduleCode <- 7734 #paper towels
modNum <- 4507
modName <- "paperTowels"
# moduleCode <- 3625 #paper towels
# modNum <- 2506
# modName <- "milk"
path <- "/scratch/upenn/hossaine/nielsen_extracts/RMS/"
topBrands <- c("CTL BR", "KEMPS", "HORIZON ORGANIC", "BORDEN", "LAND O LAKES")
smallSize <- 32
medSize <- 64

# Transfer data from Globus
# cd /scratch/upenn/hossaine
# tar -xzvf tp.tar.gz

# Getting product sales across all stores
fullMod <- NULL
for (yr in yrs) {
  print(yr)
  mod <- fread(paste0(path, yr, "/Movement_Files/", modNum, "_", yr, "/", moduleCode, "_", yr, ".tsv"),
              nThread = threads,
              select = c("upc", "price", "units"))
  upcVer <- fread(paste0(path, yr, "/Annual_Files/rms_versions_", yr, ".tsv"))
  mod <- merge(mod, upcVer, by = "upc")
  fullMod <- rbindlist(list(fullMod, mod))
  rm(mod)
}

# Computing unit-weighted average price for each UPC
fullMod <- fullMod[, .(avgPrice = weighted.mean(price, w = units)),
                 by = .(upc, upc_ver_uc, panel_year)]
fwrite(fullMod, paste0("/scratch/upenn/hossaine/", modName,
                       "AssortmentInflation.csv"), nThread = threads)

# Merging with product characteristics
mod <- fread(paste0("/scratch/upenn/hossaine/", modName,
                    "AssortmentInflation.csv"), nThread = threads)

prod <- fread(paste0(path, "Master_Files/Latest/products.tsv"), nThread = threads,
              select = c("upc", "upc_ver_uc", "upc_descr", "product_module_code",
                         "brand_descr", "multi", "size1_amount"),
              quote = "")[product_module_code == moduleCode]
prod[, c("units", "product_module_code") := .(as.integer(multi * size1_amount), NULL)]
prod[, c("upc_descr", "multi", "size1_amount") := NULL]
mod <- merge(mod, prod, by = c("upc", "upc_ver_uc"))

# Running simulation
# I pick a roll size and then have the household sample 1 product from that
# package and assume that they purchase that for the whole year
mod[units <= smallSize, "size" := "Small"]
mod[units <= medSize & units > smallSize, "size" := "Medium"]
mod[units > medSize, "size" := "Large"]
mod[, "size" := factor(size, levels = c("Small", "Medium", "Large"), ordered = TRUE)]
mod[, "unitPrice" := avgPrice / units]
sims <- mod[brand_descr %in% topBrands, .SD[sample(.N, 100000, replace = TRUE)],
           by = .(brand_descr, size, panel_year)]

graphData <- sims[, .(unitPrice = mean(unitPrice)), by = .(brand_descr, size, panel_year)]
ggplot(data = graphData, aes(x = panel_year, y = unitPrice, color = size)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(brand_descr)) +
  scale_color_grey()

# Computing experienced price changes
infl <- dcast(graphData[panel_year %in% c(2006, 2016)],
              brand_descr + size ~ panel_year, value.var = "unitPrice")
infl[, "pctChange" := `2016` / `2006` * 100]

ggplot(data = infl, aes(x = brand_descr, y = pctChange)) +
  geom_bar(aes(fill = size), position = "dodge", stat = "identity") +
  labs(title = "Small Packages Have Higher Inflation",
       x = "Brand",
       y = "2016 Unit Price Index (2006 = 100)",
       caption = paste0("Source: Author calculations from Nielsen Consumer Panel. \n",
                        "Package sizes are as follows: Small (1-6 rolls), Medium (7-12 rolls) Large (>12 rolls).")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0)) +
  scale_fill_discrete()
