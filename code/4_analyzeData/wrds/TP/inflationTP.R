# Generates price series for toilet paper
library(data.table)
library(stringr)
library(ggplot2)
library(ggthemes)
yrs <- 2006:2016
threads <- 8
moduleCode <- 7260 #tp
path <- "/scratch/upenn/hossaine/nielsen_extracts/RMS/"

# Transfer data from Globus
# cd /scratch/upenn/hossaine
# tar -xzvf tp.tar.gz

# Getting toilet paper sales across all stores
fullTP <- NULL
for (yr in yrs) {
  print(yr)
  tp <- fread(paste0(path, yr, "/Movement_Files/4507_", yr, "/", moduleCode, "_", yr, ".tsv"),
              nThread = threads,
              select = c("upc", "price", "units"))
  upcVer <- fread(paste0(path, yr, "/Annual_Files/rms_versions_", yr, ".tsv"))
  tp <- merge(tp, upcVer, by = "upc")
  fullTP <- rbindlist(list(fullTP, tp))
  rm(tp)
}

# Computing unit-weighted average price for each UPC
fullTP <- fullTP[, .(avgPrice = weighted.mean(price, w = units)),
                 by = .(upc, upc_ver_uc, panel_year)]
fwrite(fullTP, "/scratch/upenn/hossaine/TPAssortmentInflation.csv", nThread = threads)

# Merging with product characteristics
tp <- fread("/scratch/upenn/hossaine/TPAssortmentInflation.csv", nThread = threads)

prod <- fread("/scratch/upenn/hossaine/prodTP.csv")
tp <- merge(tp, prod, by = c("upc", "upc_ver_uc"))

# Running simulation
# I pick a roll size and then have the household sample 1 product from that
# package and assume that they purchase that for the whole year
tp[rolls %in% 4:6, "size" := "Small"]
tp[rolls %in% 8:12, "size" := "Medium"]
tp[rolls %in% 16:36, "size" := "Large"]
tp[, "size" := factor(size, levels = c("Small", "Medium", "Large"), ordered = TRUE)]
tp[, "unitPrice" := avgPrice / sheets]
topBrands <- c("ANGEL SOFT", "CHARMIN", "KLEENEX COTTONELLE", "QUILTED NORTHERN", "SCOTT 1000")
sims <- tp[brand_descr %in% topBrands, .SD[sample(.N, 100000, replace = TRUE)],
           by = .(brand_descr, size, panel_year)]

graphData <- sims[, .(unitPrice = mean(unitPrice)), by = .(brand_descr, size, panel_year)]
baseYear <- graphData[panel_year == 2006][, "panel_year" := NULL]
setnames(baseYear, "unitPrice", "unitPrice2006")
graphData <- merge(graphData, baseYear, by = c("brand_descr", "size"))
graphData[, "index" := unitPrice / unitPrice2006 * 100]
ggplot(data = na.omit(graphData), aes(x = panel_year, y = index, color = size)) +
  geom_line() +
  geom_hline(yintercept = 100) +
  facet_wrap(vars(brand_descr)) +
  scale_color_grey() +
  labs(title = "Small Packages Have Higher Inflation",
       x = "Year",
       y = "Unit Price Index (2006 = 100)",
       caption = paste0("Source: Author calculations from Nielsen Consumer Panel. \n",
                        "Package sizes are as follows: Small (4-6 rolls), Medium (8-12 rolls) Large (16-36 rolls).")) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), plot.caption = element_text(hjust = 0))
ggsave(filename = "./figures/inflationTPTimeSeries.png")

# Computing experienced price changes
infl <- dcast(graphData[panel_year %in% c(2006, 2016)],
              brand_descr + size ~ panel_year, value.var = "unitPrice")
infl[, "pctChange" := `2016` / `2006` * 100]
infl[brand_descr == "ANGEL SOFT", "brand_descr" := "Angel Soft"]
infl[brand_descr == "CHARMIN", "brand_descr" := "Charmin"]
infl[brand_descr == "KLEENEX COTTONELLE", "brand_descr" := "Cottonelle"]
infl[brand_descr == "QUILTED NORTHERN", "brand_descr" := "Qltd Ntn"]
infl[brand_descr == "SCOTT 1000", "brand_descr" := "Scott"]

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
ggsave(filename = "./figures/inflationTP.png", height = 6, width = 6)
