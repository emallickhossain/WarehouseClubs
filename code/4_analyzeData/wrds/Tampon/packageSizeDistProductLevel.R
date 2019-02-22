# Gets package size distribution by brand for tampons
library(data.table)
library(ggplot2)
library(ggthemes)
library(ggridges)
path <- "/scratch/upenn/hossaine/"
tamponPurch <- fread(paste0(path, "tamponPurch.csv"),
                 select = c("upc", "upc_ver_uc", "brand_code_uc", "brand_descr",
                            "size", "total_price_paid_real", "channel_type",
                            "household_income_coarse", "projection_factor"))

# Getting top brands
topBrands <- tamponPurch[, .(sales = sum(total_price_paid_real)),
                     by = .(brand_code_uc, brand_descr, household_income_coarse)]
setorder(topBrands, -sales)
brands <- topBrands[, .SD[1:10], by = .(household_income_coarse)]
tampon <- tamponPurch[brand_code_uc %in% brands$brand_code_uc]
tampon[brand_code_uc == 5367466905, "brand_descr" := "DISCOUNT LABEL 2"]
tampon[brand_code_uc == 5367466920, "brand_descr" := "DISCOUNT LABEL 1"]

ggplot(tampon, aes(y = as.character(brand_descr), x = size)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_fivethirtyeight() +
  labs(x = "Package Size",
       y = "Brand",
       title = "Package Size Distribution by Brand",
       caption = "Source: Nielsen Consumer Panel")
ggsave("./figures/tamponSizeDist.png")

# Doing by store type
stores <- c("Drug Store", "Discount Store", "Grocery", "Dollar Store", "Warehouse Club")
ggplot(tamponPurch[channel_type %in% stores],
       aes(y = as.character(channel_type), x = size)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_fivethirtyeight() +
  labs(x = "Package Size",
       y = "Store Type",
       title = "Package Size Distribution by Store Type",
       caption = "Source: Nielsen Consumer Panel")
ggsave("./figures/tamponSizeDistByStoreType.png")
