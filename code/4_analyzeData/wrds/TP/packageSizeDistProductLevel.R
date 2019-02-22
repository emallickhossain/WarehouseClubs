# Gets package size distribution by brand for toilet paper
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggridges)
path <- "/scratch/upenn/hossaine/"
tpPurch <- fread(paste0(path, "7260Purch.csv"),
                 select = c("upc", "upc_ver_uc", "brand_code_uc", "brand_descr",
                            "sizeUnadj", "size", "total_price_paid_real",
                            "channel_type", "household_income_coarse", "projection_factor"))

# Getting top brands
topBrands <- tpPurch[, .(sales = sum(total_price_paid_real)),
                     by = .(brand_code_uc, brand_descr, household_income_coarse)]
setorder(topBrands, -sales)
brands <- topBrands[, .SD[1:10], by = .(household_income_coarse)]
tp <- tpPurch[brand_code_uc %in% brands$brand_code_uc]
tp[brand_code_uc == 5367469101, "brand_descr" := "WAREHOUSE LABEL 1"]
tp[brand_code_uc == 5367469103, "brand_descr" := "WAREHOUSE LABEL 2"]
tp[brand_code_uc == 5367466920, "brand_descr" := "DISCOUNT LABEL"]

ggplot(tp, aes(y = as.character(brand_descr), x = size)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_fivethirtyeight() +
  xlim(0, 60) +
  labs(x = "Package Size",
       y = "Brand",
       title = "Package Size Distribution by Brand",
       caption = "Source: Nielsen Consumer Panel") +
  theme(axis.title = element_text())
ggsave("./figures/tpSizeDist.png")

# Doing by store type
stores <- c("Drug Store", "Discount Store", "Grocery", "Dollar Store", "Warehouse Club")
ggplot(tpPurch[channel_type %in% stores],
       aes(y = as.character(channel_type), x = size)) +
  geom_density_ridges(rel_min_height = 0.01) +
  xlim(0, 75) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())
  labs(x = "Package Size",
       y = "Store Type",
       title = "Package Size Distribution by Store Type",
       caption = paste0("Source: Author calulations using Nielsen Consumer Panel.\n",
                        "Note: TP rolls are standardized to 275-sheet, 2-ply rolls."))
ggsave("./figures/tpSizeDistByStoreType.png")
